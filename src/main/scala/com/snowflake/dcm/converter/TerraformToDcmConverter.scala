package com.snowflake.dcm.converter

import com.snowflake.dcm.dcm._
import com.snowflake.dcm.hcl.{HclBlock, HclValue}
import com.snowflake.dcm.terraform.{TerraformProject, TerraformResource}

/**
 * Converts every resource in a TerraformProject to a list of DcmStatements.
 *
 * Supported Terraform resource types:
 *   snowflake_database
 *   snowflake_schema
 *   snowflake_warehouse
 *   snowflake_account_role
 *   snowflake_table
 *   snowflake_view
 *   snowflake_dynamic_table
 *   snowflake_grant_privileges_to_account_role
 *   snowflake_grant_account_role
 *   snowflake_grant_database_role  (partial)
 *   snowflake_grant_ownership
 */
class TerraformToDcmConverter(project: TerraformProject) {

  private val resolver = ReferenceResolver(project)

  def convert(): List[DcmStatement] = {
    // Conversion order follows DCM creation-order dependency:
    //   databases → schemas → warehouses → roles → tables → views → dynamic tables → grants
    val databases     = convertAll("snowflake_database",                          convertDatabase)
    val schemas       = convertAll("snowflake_schema",                            convertSchema)
    val warehouses    = convertAll("snowflake_warehouse",                         convertWarehouse)
    val roles         = convertAll("snowflake_role",                               convertRole) ++
                        convertAll("snowflake_account_role",                       convertRole)
    val tables        = convertAll("snowflake_table",                             convertTable)
    val views         = convertAll("snowflake_view",                              convertView)
    val dynamicTables = convertAll("snowflake_dynamic_table",                     convertDynamicTable)
    val privGrants    = convertAll("snowflake_grant_privileges_to_role",          convertPrivilegeGrant) ++
                        convertAll("snowflake_grant_privileges_to_account_role",  convertPrivilegeGrant)
    val roleGrants    = convertAll("snowflake_grant_role",                        convertRoleGrant) ++
                        convertAll("snowflake_grant_account_role",                convertRoleGrant)
    val dbRoleGrants  = convertAll("snowflake_grant_database_role",               convertDatabaseRoleGrant)
    val ownGrants     = convertAll("snowflake_grant_ownership",                   convertOwnershipGrant)

    databases ++ schemas ++ warehouses ++ roles ++ tables ++ views ++ dynamicTables ++
      privGrants ++ roleGrants ++ dbRoleGrants ++ ownGrants
  }

  private def convertAll(rType: String, fn: TerraformResource => List[DcmStatement]): List[DcmStatement] =
    project.resourcesOfType(rType).flatMap(fn)

  // ─── Database ─────────────────────────────────────────────────────────────

  private def convertDatabase(r: TerraformResource): List[DcmStatement] = List(
    DefineDatabase(
      name                       = req(r, "name"),
      isTransient                = r.bool("is_transient").getOrElse(false),
      dataRetentionDays          = r.num("data_retention_time_in_days").map(_.toInt),
      maxDataExtensionDays       = r.num("max_data_extension_time_in_days").map(_.toInt),
      defaultDdlCollation        = r.str("default_ddl_collation"),
      storageSerializationPolicy = r.str("storage_serialization_policy"),
      logLevel                   = r.str("log_level"),
      traceLevel                 = r.str("trace_level"),
      comment                    = r.str("comment")
    )
  )

  // ─── Schema ───────────────────────────────────────────────────────────────

  private def convertSchema(r: TerraformResource): List[DcmStatement] = {
    val db     = resolve(r, "database")
    val schema = req(r, "name")
    val fqn    = if (db.nonEmpty) s"$db.$schema" else schema
    List(DefineSchema(
      name               = fqn,
      isTransient        = r.bool("is_transient").getOrElse(false),
      withManagedAccess  = r.bool("with_managed_access").getOrElse(false),
      dataRetentionDays  = r.num("data_retention_time_in_days").map(_.toInt),
      defaultDdlCollation= r.str("default_ddl_collation"),
      comment            = r.str("comment")
    ))
  }

  // ─── Warehouse ────────────────────────────────────────────────────────────

  private def convertWarehouse(r: TerraformResource): List[DcmStatement] = List(
    DefineWarehouse(
      name                           = req(r, "name"),
      warehouseType                  = r.str("warehouse_type"),
      warehouseSize                  = r.str("warehouse_size"),
      maxClusterCount                = r.num("max_cluster_count").map(_.toInt),
      minClusterCount                = r.num("min_cluster_count").map(_.toInt),
      scalingPolicy                  = r.str("scaling_policy"),
      autoSuspend                    = r.num("auto_suspend").map(_.toInt),
      autoResume                     = r.bool("auto_resume"),
      initiallySuspended             = r.bool("initially_suspended"),
      enableQueryAcceleration        = r.bool("enable_query_acceleration"),
      queryAccelerationMaxScaleFactor= r.num("query_acceleration_max_scale_factor").map(_.toInt),
      resourceMonitor                = r.str("resource_monitor"),
      comment                        = r.str("comment")
    )
  )

  // ─── Role ─────────────────────────────────────────────────────────────────

  private def convertRole(r: TerraformResource): List[DcmStatement] = List(
    DefineRole(
      name    = req(r, "name"),
      comment = r.str("comment")
    )
  )

  // ─── Table ────────────────────────────────────────────────────────────────

  private def convertTable(r: TerraformResource): List[DcmStatement] = {
    val db     = resolve(r, "database")
    val schema = resolve(r, "schema")
    val table  = req(r, "name")
    val fqn    = qualifyName(db, schema, table)

    val columns = r.allBlocks("column").map { col =>
      val colBody = col.body
      ColumnDef(
        name         = colBody.strAttr("name").getOrElse("UNKNOWN"),
        dataType     = colBody.strAttr("type").getOrElse("VARIANT"),
        nullable     = colBody.attr("nullable").collect { case HclValue.Bool(b) => b }.getOrElse(true),
        defaultValue = colBody.block("default").flatMap(_.body.strAttr("expression")),
        comment      = colBody.strAttr("comment")
      )
    }

    List(DefineTable(
      name              = fqn,
      isTransient       = r.bool("is_transient").getOrElse(false),
      columns           = columns,
      dataRetentionDays = r.num("data_retention_time_in_days").map(_.toInt),
      comment           = r.str("comment")
    ))
  }

  // ─── View ─────────────────────────────────────────────────────────────────

  private def convertView(r: TerraformResource): List[DcmStatement] = {
    val db    = resolve(r, "database")
    val sch   = resolve(r, "schema")
    val view  = req(r, "name")
    val fqn   = qualifyName(db, sch, view)
    val query = r.attr("statement").map(resolver.resolve)
      .orElse(r.attr("query").map(resolver.resolve))
      .getOrElse("-- TODO: add SELECT query")

    List(DefineView(name = fqn, query = query, comment = r.str("comment")))
  }

  // ─── Dynamic Table ────────────────────────────────────────────────────────

  private def convertDynamicTable(r: TerraformResource): List[DcmStatement] = {
    val db  = resolve(r, "database")
    val sch = resolve(r, "schema")
    val fqn = qualifyName(db, sch, req(r, "name"))
    val wh  = resolve(r, "warehouse")

    val targetLag: String = r.block("target_lag").map { b =>
      b.body.attr("maximum_duration").map(HclValue.asString)
        .orElse(b.body.attr("downstream").collect { case HclValue.Bool(true) => "DOWNSTREAM" })
        .getOrElse("1 minute")
    }.orElse(r.str("target_lag")).getOrElse("1 minute")

    val query = r.attr("query").map(resolver.resolve).getOrElse("-- TODO: add SELECT query")

    List(DefineDynamicTable(
      name      = fqn,
      targetLag = targetLag,
      warehouse = wh,
      query     = query,
      comment   = r.str("comment")
    ))
  }

  // ─── Grant: privileges to account role ────────────────────────────────────

  private def convertPrivilegeGrant(r: TerraformResource): List[DcmStatement] = {
    val role = { val v = resolve(r, "account_role_name"); if (v.nonEmpty) v else resolve(r, "role_name") }
    val allPrivs   = r.bool("all_privileges").getOrElse(false)
    val grantOpt   = r.bool("with_grant_option").getOrElse(false)
    val privileges = if (allPrivs) List("ALL PRIVILEGES") else r.list("privileges")

    // Build one statement per grant target block
    val onAccount = r.bool("on_account").getOrElse(false)
    if (onAccount) {
      return List(GrantPrivileges(privileges, "ACCOUNT", "", role, grantOpt))
    }

    r.block("on_account_object").map { b =>
      val objType = b.body.strAttr("object_type").getOrElse("DATABASE")
      val objName = b.body.attr("object_name").map(resolver.resolve).getOrElse("")
      List(GrantPrivileges(privileges, objType, objName, role, grantOpt))
    }.orElse(
      r.block("on_schema").map { b =>
        schemaGrantStatements(b, privileges, role, grantOpt)
      }
    ).orElse(
      r.block("on_schema_object").map { b =>
        schemaObjectGrantStatements(b, privileges, role, grantOpt)
      }
    ).getOrElse(Nil)
  }

  private def schemaGrantStatements(
      block: HclBlock,
      privileges: List[String],
      role: String,
      grantOpt: Boolean
  ): List[DcmStatement] = {
    val b = block.body
    b.attr("schema_name").map(v =>
      List(GrantPrivileges(privileges, "SCHEMA", resolver.resolve(v), role, grantOpt))
    ).orElse(
      b.attr("all_schemas_in_database").map(v =>
        List(GrantOnAll(privileges, "SCHEMAS", "DATABASE", resolver.resolve(v), role))
      )
    ).orElse(
      b.attr("future_schemas_in_database").map(v =>
        List(GrantOnFuture(privileges, "SCHEMAS", "DATABASE", resolver.resolve(v), role))
      )
    ).getOrElse(Nil)
  }

  private def schemaObjectGrantStatements(
      block: HclBlock,
      privileges: List[String],
      role: String,
      grantOpt: Boolean
  ): List[DcmStatement] = {
    val b = block.body

    // on_schema_object { object_type = "TABLE" object_name = "..." }
    val directGrant = for {
      objType <- b.strAttr("object_type")
      objName <- b.attr("object_name").map(resolver.resolve)
    } yield List(GrantPrivileges(privileges, objType, objName, role, grantOpt))

    directGrant.orElse(
      b.block("all").map { allBlock =>
        val plural   = allBlock.body.strAttr("object_type_plural").getOrElse("TABLES")
        val inDb     = allBlock.body.attr("in_database").map(resolver.resolve)
        val inSchema = allBlock.body.attr("in_schema").map(resolver.resolve)
        val (ct, cn) = inDb.map("DATABASE" -> _).orElse(inSchema.map("SCHEMA" -> _)).getOrElse("DATABASE" -> "")
        List(GrantOnAll(privileges, plural, ct, cn, role))
      }
    ).orElse(
      b.block("future").map { futBlock =>
        val plural   = futBlock.body.strAttr("object_type_plural").getOrElse("TABLES")
        val inDb     = futBlock.body.attr("in_database").map(resolver.resolve)
        val inSchema = futBlock.body.attr("in_schema").map(resolver.resolve)
        val (ct, cn) = inDb.map("DATABASE" -> _).orElse(inSchema.map("SCHEMA" -> _)).getOrElse("DATABASE" -> "")
        List(GrantOnFuture(privileges, plural, ct, cn, role))
      }
    ).getOrElse(Nil)
  }

  // ─── Grant: role to role ──────────────────────────────────────────────────

  private def convertRoleGrant(r: TerraformResource): List[DcmStatement] = {
    val child      = resolve(r, "role_name")
    val parentRole = resolve(r, "parent_role_name")
    val userName   = resolve(r, "user_name")

    if (parentRole.nonEmpty) {
      List(GrantRole(child, parentRole))
    } else if (userName.nonEmpty) {
      // GRANT ROLE ... TO USER is not a DCM-managed statement — emit as a comment
      List(UnsupportedResource(
        r.resourceType, r.resourceName,
        s"GRANT ROLE $child TO USER $userName  -- role-to-user grants are not managed by DCM"
      ))
    } else {
      List(UnsupportedResource(r.resourceType, r.resourceName, "missing parent_role_name and user_name"))
    }
  }

  // ─── Grant: database role (simplified) ───────────────────────────────────

  private def convertDatabaseRoleGrant(r: TerraformResource): List[DcmStatement] = {
    val dbRole = resolve(r, "database_role_name")
    val parent = resolve(r, "parent_role_name")
    List(GrantRole(dbRole, parent))
  }

  // ─── Grant: ownership ─────────────────────────────────────────────────────

  private def convertOwnershipGrant(r: TerraformResource): List[DcmStatement] = {
    val role = { val v = resolve(r, "account_role_name"); if (v.nonEmpty) v else resolve(r, "role_name") }
    r.block("on").map { b =>
      val objType = b.body.strAttr("object_type").getOrElse("DATABASE")
      val objName = b.body.attr("object_name").map(resolver.resolve).getOrElse("")
      List(GrantOwnership(objType, objName, role))
    }.getOrElse(Nil)
  }

  // ─── Helpers ──────────────────────────────────────────────────────────────

  /** Resolve a resource attribute to a string (follows references). */
  private def resolve(r: TerraformResource, attr: String): String =
    r.attr(attr).map(resolver.resolve).getOrElse("")

  /** Resolve an attribute that must be present; warn if missing. */
  private def req(r: TerraformResource, attr: String): String =
    r.attr(attr).map(resolver.resolve).getOrElse {
      System.err.println(s"[WARN] Missing required attribute '$attr' on ${r.resourceType}.${r.resourceName}")
      s"UNKNOWN_${attr.toUpperCase}"
    }

  private def qualifyName(db: String, schema: String, name: String): String =
    (db, schema) match {
      case ("", "") => name
      case ("", s)  => s"$s.$name"
      case (d, "")  => s"$d.$name"
      case (d, s)   => s"$d.$s.$name"
    }
}

object TerraformToDcmConverter {
  def convert(project: TerraformProject): List[DcmStatement] =
    new TerraformToDcmConverter(project).convert()
}
