package com.snowflake.dcm

import com.snowflake.dcm.converter.TerraformToDcmConverter
import com.snowflake.dcm.dcm._
import com.snowflake.dcm.terraform.TerraformLoader
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ConverterSpec extends AnyFlatSpec with Matchers {

  private def convert(hcl: String): List[DcmStatement] = {
    val project = TerraformLoader.loadString(hcl)
    TerraformToDcmConverter.convert(project)
  }

  // ─── Database ─────────────────────────────────────────────────────────────

  "TerraformToDcmConverter" should "convert snowflake_database" in {
    val stmts = convert(
      """resource "snowflake_database" "my_db" {
        |  name                        = "MY_DB"
        |  data_retention_time_in_days = 7
        |  comment                     = "My database"
        |}""".stripMargin
    )
    stmts should have size 1
    stmts.head shouldBe DefineDatabase(
      name              = "MY_DB",
      dataRetentionDays = Some(7),
      comment           = Some("My database")
    )
  }

  it should "convert transient database" in {
    val stmts = convert(
      """resource "snowflake_database" "td" {
        |  name         = "TEMP_DB"
        |  is_transient = true
        |}""".stripMargin
    )
    stmts.head.asInstanceOf[DefineDatabase].isTransient shouldBe true
  }

  // ─── Schema ───────────────────────────────────────────────────────────────

  it should "convert snowflake_schema with fully-qualified name" in {
    val stmts = convert(
      """resource "snowflake_schema" "my_schema" {
        |  database            = "MY_DB"
        |  name                = "MY_SCHEMA"
        |  with_managed_access = true
        |}""".stripMargin
    )
    stmts.head shouldBe DefineSchema(
      name              = "MY_DB.MY_SCHEMA",
      withManagedAccess = true
    )
  }

  // ─── Warehouse ────────────────────────────────────────────────────────────

  it should "convert snowflake_warehouse" in {
    val stmts = convert(
      """resource "snowflake_warehouse" "wh" {
        |  name           = "MY_WH"
        |  warehouse_size = "MEDIUM"
        |  auto_suspend   = 300
        |  auto_resume    = true
        |}""".stripMargin
    )
    stmts.head shouldBe DefineWarehouse(
      name          = "MY_WH",
      warehouseSize = Some("MEDIUM"),
      autoSuspend   = Some(300),
      autoResume    = Some(true)
    )
  }

  // ─── Role ─────────────────────────────────────────────────────────────────

  it should "convert snowflake_account_role" in {
    val stmts = convert(
      """resource "snowflake_account_role" "admin" {
        |  name    = "ADMIN_ROLE"
        |  comment = "Admin role"
        |}""".stripMargin
    )
    stmts.head shouldBe DefineRole("ADMIN_ROLE", Some("Admin role"))
  }

  // ─── Table ────────────────────────────────────────────────────────────────

  it should "convert snowflake_table with columns" in {
    val stmts = convert(
      """resource "snowflake_table" "t" {
        |  database = "MY_DB"
        |  schema   = "MY_SCHEMA"
        |  name     = "MY_TABLE"
        |  comment  = "My table"
        |  column {
        |    name     = "ID"
        |    type     = "NUMBER(38,0)"
        |    nullable = false
        |  }
        |  column {
        |    name = "EMAIL"
        |    type = "VARCHAR(256)"
        |  }
        |}""".stripMargin
    )
    val table = stmts.head.asInstanceOf[DefineTable]
    table.name    shouldBe "MY_DB.MY_SCHEMA.MY_TABLE"
    table.comment shouldBe Some("My table")
    table.columns should have size 2
    table.columns.head shouldBe ColumnDef("ID", "NUMBER(38,0)", nullable = false)
    table.columns(1) shouldBe ColumnDef("EMAIL", "VARCHAR(256)", nullable = true)
  }

  // ─── View ─────────────────────────────────────────────────────────────────

  it should "convert snowflake_view" in {
    val stmts = convert(
      """resource "snowflake_view" "v" {
        |  database  = "MY_DB"
        |  schema    = "MY_SCHEMA"
        |  name      = "MY_VIEW"
        |  statement = "SELECT id, name FROM MY_DB.MY_SCHEMA.MY_TABLE WHERE active = TRUE"
        |}""".stripMargin
    )
    val view = stmts.head.asInstanceOf[DefineView]
    view.name  shouldBe "MY_DB.MY_SCHEMA.MY_VIEW"
    view.query shouldBe "SELECT id, name FROM MY_DB.MY_SCHEMA.MY_TABLE WHERE active = TRUE"
  }

  // ─── Dynamic Table ────────────────────────────────────────────────────────

  it should "convert snowflake_dynamic_table" in {
    val stmts = convert(
      """resource "snowflake_dynamic_table" "dt" {
        |  database  = "MY_DB"
        |  schema    = "MY_SCHEMA"
        |  name      = "MY_DT"
        |  warehouse = "MY_WH"
        |  query     = "SELECT * FROM MY_DB.MY_SCHEMA.SRC"
        |  target_lag {
        |    maximum_duration = "5 minutes"
        |  }
        |}""".stripMargin
    )
    val dt = stmts.head.asInstanceOf[DefineDynamicTable]
    dt.name      shouldBe "MY_DB.MY_SCHEMA.MY_DT"
    dt.warehouse shouldBe "MY_WH"
    dt.targetLag shouldBe "5 minutes"
  }

  // ─── Privilege grants ─────────────────────────────────────────────────────

  it should "convert on_schema_object privilege grant" in {
    val stmts = convert(
      """resource "snowflake_grant_privileges_to_account_role" "g" {
        |  account_role_name = "MY_ROLE"
        |  privileges        = ["SELECT", "INSERT"]
        |  on_schema_object {
        |    object_type = "TABLE"
        |    object_name = "MY_DB.MY_SCHEMA.MY_TABLE"
        |  }
        |}""".stripMargin
    )
    stmts.head shouldBe GrantPrivileges(
      privileges  = List("SELECT", "INSERT"),
      objectType  = "TABLE",
      objectName  = "MY_DB.MY_SCHEMA.MY_TABLE",
      roleName    = "MY_ROLE"
    )
  }

  it should "convert all_privileges on database" in {
    val stmts = convert(
      """resource "snowflake_grant_privileges_to_account_role" "g" {
        |  account_role_name = "MY_ROLE"
        |  all_privileges    = true
        |  on_account_object {
        |    object_type = "DATABASE"
        |    object_name = "MY_DB"
        |  }
        |}""".stripMargin
    )
    stmts.head shouldBe GrantPrivileges(
      privileges = List("ALL PRIVILEGES"),
      objectType = "DATABASE",
      objectName = "MY_DB",
      roleName   = "MY_ROLE"
    )
  }

  it should "convert future tables grant" in {
    val stmts = convert(
      """resource "snowflake_grant_privileges_to_account_role" "g" {
        |  account_role_name = "MY_ROLE"
        |  privileges = ["SELECT"]
        |  on_schema_object {
        |    future {
        |      object_type_plural = "TABLES"
        |      in_schema          = "MY_DB.MY_SCHEMA"
        |    }
        |  }
        |}""".stripMargin
    )
    stmts.head shouldBe GrantOnFuture(
      privileges       = List("SELECT"),
      objectTypePlural = "TABLES",
      containerType    = "SCHEMA",
      containerName    = "MY_DB.MY_SCHEMA",
      roleName         = "MY_ROLE"
    )
  }

  // ─── Role grants ──────────────────────────────────────────────────────────

  it should "convert snowflake_grant_account_role" in {
    val stmts = convert(
      """resource "snowflake_grant_account_role" "g" {
        |  role_name        = "CHILD_ROLE"
        |  parent_role_name = "SYSADMIN"
        |}""".stripMargin
    )
    stmts.head shouldBe GrantRole("CHILD_ROLE", "SYSADMIN")
  }

  // ─── Ownership grants ─────────────────────────────────────────────────────

  it should "convert snowflake_grant_ownership" in {
    val stmts = convert(
      """resource "snowflake_grant_ownership" "own" {
        |  account_role_name = "MY_ROLE"
        |  on {
        |    object_type = "DATABASE"
        |    object_name = "MY_DB"
        |  }
        |}""".stripMargin
    )
    stmts.head shouldBe GrantOwnership("DATABASE", "MY_DB", "MY_ROLE")
  }

  // ─── Reference resolution ────────────────────────────────────────────────

  it should "resolve cross-resource references" in {
    val hcl =
      """resource "snowflake_database" "db" { name = "MY_DB" }
        |resource "snowflake_schema" "sch" {
        |  database = snowflake_database.db.name
        |  name     = "MY_SCHEMA"
        |}""".stripMargin
    val stmts = convert(hcl)
    val schema = stmts.collect { case s: DefineSchema => s }.head
    schema.name shouldBe "MY_DB.MY_SCHEMA"
  }

  it should "resolve var references to defaults" in {
    val hcl =
      """variable "env" { default = "PROD" }
        |resource "snowflake_database" "db" { name = "${var.env}_DB" }""".stripMargin
    val stmts = convert(hcl)
    val db = stmts.collect { case s: DefineDatabase => s }.head
    db.name shouldBe "PROD_DB"
  }

  // ─── Renderer smoke tests ─────────────────────────────────────────────────

  "DcmRenderer" should "render DefineDatabase" in {
    val sql = com.snowflake.dcm.dcm.DcmRenderer.render(DefineDatabase(
      name = "MY_DB",
      dataRetentionDays = Some(7),
      comment = Some("test db")
    ))
    sql should include("DEFINE DATABASE MY_DB")
    sql should include("DATA_RETENTION_TIME_IN_DAYS = 7")
    sql should include("COMMENT = 'test db'")
  }

  it should "render DefineTable with columns" in {
    val sql = com.snowflake.dcm.dcm.DcmRenderer.render(DefineTable(
      name = "MY_DB.MY_SCHEMA.MY_TABLE",
      columns = List(
        ColumnDef("ID", "NUMBER(38,0)", nullable = false),
        ColumnDef("NAME", "VARCHAR(100)", nullable = true)
      ),
      comment = Some("my table")
    ))
    sql should include("DEFINE TABLE MY_DB.MY_SCHEMA.MY_TABLE")
    sql should include("ID NUMBER(38,0) NOT NULL")
    sql should include("NAME VARCHAR(100)")
  }

  it should "render GrantOnFuture" in {
    val sql = com.snowflake.dcm.dcm.DcmRenderer.render(GrantOnFuture(
      privileges       = List("SELECT"),
      objectTypePlural = "TABLES",
      containerType    = "SCHEMA",
      containerName    = "MY_DB.MY_SCHEMA",
      roleName         = "MY_ROLE"
    ))
    sql shouldBe "GRANT SELECT ON FUTURE TABLES IN SCHEMA MY_DB.MY_SCHEMA TO ROLE MY_ROLE;"
  }

  it should "render GrantRole" in {
    val sql = com.snowflake.dcm.dcm.DcmRenderer.render(GrantRole("CHILD", "PARENT"))
    sql shouldBe "GRANT ROLE CHILD TO ROLE PARENT;"
  }
}
