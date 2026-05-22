package com.snowflake.dcm.dcm

import com.snowflake.dcm.applier.ApplierDCM.createProject
import com.snowflake.dcm.terraform.TerraformProject

import java.io.File
import java.nio.file.{Files, Paths}
import java.nio.charset.StandardCharsets

/**
 * Writes a DCM project directory from a list of DcmStatements.
 *
 * Output structure:
 *
 *   <outputDir>/
 *     manifest.yml
 *     definitions/
 *       01_databases.sql
 *       02_schemas.sql
 *       03_warehouses.sql
 *       04_roles.sql
 *       05_tables.sql
 *       06_views.sql
 *       07_dynamic_tables.sql
 *       08_grants.sql
 *
 * Files that contain no statements are not written.
 */
object DcmProjectGenerator {

  case class DcmTarget(
    accountIdentifier: String,
    projectName:       String
  )

  case class GeneratorConfig(
    projectName:   String                  = "migrated_project",
    description:   String                  = "Migrated from Terraform",
    defaultTarget: String                  = "DCM_DEV",
    targets:       Map[String, DcmTarget]  = Map.empty
  )

  def generate(
    statements:  List[DcmStatement],
    outputDir:   File,
    project:     TerraformProject,
    config:      GeneratorConfig = GeneratorConfig()
  ): Unit = {
    outputDir.mkdirs()
    val defsDir = new File(outputDir, "sources/definitions")
    defsDir.mkdirs()

    // Partition statements by kind
    val databases     = statements.collect { case s: DefineDatabase     => s }
    val schemas       = statements.collect { case s: DefineSchema       => s }
    val warehouses    = statements.collect { case s: DefineWarehouse    => s }
    val roles         = statements.collect { case s: DefineRole         => s }
    val tables        = statements.collect { case s: DefineTable        => s }
    val views         = statements.collect { case s: DefineView         => s }
    val dynamicTables = statements.collect { case s: DefineDynamicTable => s }
    val grants: List[DcmStatement] = statements.collect {
      case s: GrantPrivileges    => s
      case s: GrantRole          => s
      case s: GrantOwnership     => s
      case s: GrantAllPrivileges => s
      case s: GrantOnAll         => s
      case s: GrantOnFuture      => s
    }
    val unsupported = statements.collect { case s: UnsupportedResource => s }

    val sections: List[(String, List[DcmStatement])] = List(
      "01_databases.sql"      -> databases.map(identity),
      "02_schemas.sql"        -> schemas.map(identity),
      "03_warehouses.sql"     -> warehouses.map(identity),
      "04_roles.sql"          -> roles.map(identity),
      "05_tables.sql"         -> tables.map(identity),
      "06_views.sql"          -> views.map(identity),
      "07_dynamic_tables.sql" -> dynamicTables.map(identity),
      "08_grants.sql"         -> grants
    )

    val writtenFiles = sections.flatMap { case (filename, stmts) =>
      if (stmts.isEmpty) None
      else {
        val content = buildSqlFile(filename, stmts)
        writeFile(new File(defsDir, filename), content)
        Some(s"sources/definitions/$filename")
      }
    }

    if (unsupported.nonEmpty) {
      val content = unsupported.map(DcmRenderer.render).mkString("\n\n")
      writeFile(new File(defsDir, "00_unsupported.sql"), header("00_unsupported.sql") + content + "\n")
    }

    // Generate manifest.yml
    val variables = project.variables.map { case (name, v) =>
      val default = v.default.map(d => s" ${com.snowflake.dcm.hcl.HclValue.asString(d)}").getOrElse(" ~")
      s"  $name:$default"
    }.toList.sorted

    val manifest = buildManifest(config, writtenFiles, variables)
    writeFile(new File(outputDir, "manifest.yml"), manifest)

    // Print summary
    println(s"\n[DCM Generator] Output written to: ${outputDir.getAbsolutePath}")
    println(s"  manifest.yml")
    (writtenFiles ++ (if (unsupported.nonEmpty) List("sources/definitions/00_unsupported.sql") else Nil))
      .foreach(f => println(s"  $f"))
    println(s"\n  Total statements: ${statements.size}")
    println(s"    databases:      ${databases.size}")
    println(s"    schemas:        ${schemas.size}")
    println(s"    warehouses:     ${warehouses.size}")
    println(s"    roles:          ${roles.size}")
    println(s"    tables:         ${tables.size}")
    println(s"    views:          ${views.size}")
    println(s"    dynamic tables: ${dynamicTables.size}")
    println(s"    grants:         ${grants.size}")
    if (unsupported.nonEmpty)
      println(s"    unsupported:    ${unsupported.size}  (see sources/definitions/00_unsupported.sql)")

    // Generation of DCM

    // createProject("")
  }

  // ─── File content builders ────────────────────────────────────────────────

  private def buildSqlFile(filename: String, stmts: List[DcmStatement]): String = {
    val sb = new StringBuilder
    sb.append(header(filename))
    stmts.foreach { s =>
      sb.append(DcmRenderer.render(s))
      sb.append("\n\n")
    }
    sb.toString()
  }

  private def header(filename: String): String =
    s"""-- =============================================================================
-- DCM Project Definition: $filename
-- Generated by terraform-to-dcm
-- =============================================================================

"""

  private def buildManifest(
    config:       GeneratorConfig,
    files:        List[String],
    variables:    List[String]
  ): String = {
    val varBlock = if (variables.isEmpty) ""
    else s"\nvariables:\n${variables.mkString("\n")}\n"

    val targetsBlock = if (config.targets.isEmpty) ""
    else {
      val entries = config.targets.map { case (name, t) =>
        s"""  $name:
    account_identifier: ${t.accountIdentifier}
    project_name: ${t.projectName}"""
      }.mkString("\n")
      s"\ntargets:\n$entries\n"
    }

    val defaultTargetLine = s"\ndefault_target: ${config.defaultTarget}\n"

    s"""# =============================================================================
# DCM Project Manifest
# Generated by terraform-to-dcm
# =============================================================================
#
# USAGE:
#   1. Load these files into Snowflake Git integration or stage
#   2. Create the DCM project object:
#        CREATE DCM PROJECT <db>.<schema>.${config.projectName};
#   3. Preview changes:
#        EXECUTE DCM PROJECT <db>.<schema>.${config.projectName} PLAN;
#   4. Deploy:
#        EXECUTE DCM PROJECT <db>.<schema>.${config.projectName} DEPLOY;
#
# To pass variables at runtime:
#        EXECUTE DCM PROJECT ... PLAN
#          USING CONFIGURATION default (environment => 'prod');
# =============================================================================

manifest_version: 2

type: DCM_PROJECT
$defaultTargetLine$targetsBlock$varBlock"""
  }

  // ─── I/O ──────────────────────────────────────────────────────────────────

  private def writeFile(file: File, content: String): Unit = {
    Files.write(file.toPath, content.getBytes(StandardCharsets.UTF_8))
  }
}
