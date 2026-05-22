package com.snowflake.dcm

import com.snowflake.dcm.hcl.{HclParser, HclValue, ExprPart, LiteralPart}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class HclParserSpec extends AnyFlatSpec with Matchers {

  // ─── Basic attribute types ─────────────────────────────────────────────────

  "HclParser" should "parse a simple string attribute" in {
    val hcl = """resource "snowflake_database" "my_db" { name = "MY_DB" }"""
    val file = HclParser.parse(hcl)
    file.blocks should have size 1
    val block = file.blocks.head
    block.blockType shouldBe "resource"
    block.labels shouldBe List("snowflake_database", "my_db")
    block.body.attributes("name") shouldBe HclValue.Str("MY_DB")
  }

  it should "parse numeric attributes" in {
    val hcl = """resource "snowflake_database" "db" { data_retention_time_in_days = 7 }"""
    val file = HclParser.parse(hcl)
    file.blocks.head.body.attributes("data_retention_time_in_days") shouldBe HclValue.Num(7.0)
  }

  it should "parse boolean attributes" in {
    val hcl = """resource "snowflake_warehouse" "wh" { auto_resume = true auto_suspend = false }"""
    val file  = HclParser.parse(hcl)
    val attrs = file.blocks.head.body.attributes
    attrs("auto_resume")  shouldBe HclValue.Bool(true)
    attrs("auto_suspend") shouldBe HclValue.Bool(false)
  }

  it should "parse null attributes" in {
    val hcl = """resource "foo" "bar" { x = null }"""
    HclParser.parse(hcl).blocks.head.body.attributes("x") shouldBe HclValue.Null
  }

  // ─── Collections ──────────────────────────────────────────────────────────

  it should "parse list attributes" in {
    val hcl = """resource "r" "n" { privileges = ["SELECT", "INSERT", "UPDATE"] }"""
    val v   = HclParser.parse(hcl).blocks.head.body.attributes("privileges")
    v shouldBe HclValue.Lst(List(HclValue.Str("SELECT"), HclValue.Str("INSERT"), HclValue.Str("UPDATE")))
  }

  it should "parse object attributes" in {
    val hcl = """resource "r" "n" { tags = { env = "dev" } }"""
    val v   = HclParser.parse(hcl).blocks.head.body.attributes("tags")
    v shouldBe HclValue.Obj(Map("env" -> HclValue.Str("dev")))
  }

  // ─── References ───────────────────────────────────────────────────────────

  it should "parse dotted references" in {
    val hcl = """resource "r" "n" { database = snowflake_database.my_db.name }"""
    val v   = HclParser.parse(hcl).blocks.head.body.attributes("database")
    v shouldBe HclValue.Ref(List("snowflake_database", "my_db", "name"))
  }

  it should "parse var references" in {
    val hcl = """resource "r" "n" { database = var.db_name }"""
    val v   = HclParser.parse(hcl).blocks.head.body.attributes("database")
    v shouldBe HclValue.Ref(List("var", "db_name"))
  }

  // ─── Template strings ─────────────────────────────────────────────────────

  it should "parse interpolated strings" in {
    val hcl = """resource "r" "n" { name = "${var.env}_DB" }"""
    val v   = HclParser.parse(hcl).blocks.head.body.attributes("name")
    v shouldBe HclValue.TemplateStr(List(ExprPart("var.env"), LiteralPart("_DB")))
  }

  it should "parse plain strings without interpolation as TString" in {
    val hcl = """resource "r" "n" { name = "PROD_DB" }"""
    val v   = HclParser.parse(hcl).blocks.head.body.attributes("name")
    v shouldBe HclValue.Str("PROD_DB")
  }

  // ─── Nested blocks ────────────────────────────────────────────────────────

  it should "parse nested blocks (column)" in {
    val hcl =
      """resource "snowflake_table" "t" {
        |  name = "MY_TABLE"
        |  column {
        |    name = "ID"
        |    type = "NUMBER(38,0)"
        |    nullable = false
        |  }
        |  column {
        |    name = "NAME"
        |    type = "VARCHAR(100)"
        |  }
        |}""".stripMargin
    val file  = HclParser.parse(hcl)
    val block = file.blocks.head
    block.body.attributes("name") shouldBe HclValue.Str("MY_TABLE")
    val cols = block.body.blocks.filter(_.blockType == "column")
    cols should have size 2
    cols.head.body.attributes("name") shouldBe HclValue.Str("ID")
    cols.head.body.attributes("nullable") shouldBe HclValue.Bool(false)
  }

  it should "parse deeply nested blocks" in {
    val hcl =
      """resource "snowflake_grant_privileges_to_account_role" "g" {
        |  account_role_name = "MY_ROLE"
        |  privileges = ["SELECT"]
        |  on_schema_object {
        |    object_type = "TABLE"
        |    object_name = "MY_DB.MY_SCHEMA.MY_TABLE"
        |  }
        |}""".stripMargin
    val file  = HclParser.parse(hcl)
    val block = file.blocks.head
    val nested = block.body.blocks.find(_.blockType == "on_schema_object")
    nested should be(defined)
    nested.get.body.attributes("object_type") shouldBe HclValue.Str("TABLE")
  }

  // ─── Comments ─────────────────────────────────────────────────────────────

  it should "skip line comments (#)" in {
    val hcl =
      """# top comment
        |resource "r" "n" {
        |  name = "X"  # inline comment
        |}""".stripMargin
    val file = HclParser.parse(hcl)
    file.blocks should have size 1
    file.blocks.head.body.attributes("name") shouldBe HclValue.Str("X")
  }

  it should "skip line comments (//)" in {
    val hcl =
      """// another comment
        |resource "r" "n" {
        |  name = "Y" // end comment
        |}""".stripMargin
    HclParser.parse(hcl).blocks.head.body.attributes("name") shouldBe HclValue.Str("Y")
  }

  it should "skip block comments" in {
    val hcl = """resource /* block */ "r" "n" { name = "Z" }"""
    HclParser.parse(hcl).blocks.head.body.attributes("name") shouldBe HclValue.Str("Z")
  }

  // ─── Variables ────────────────────────────────────────────────────────────

  it should "parse variable blocks" in {
    val hcl =
      """variable "environment" {
        |  type    = string
        |  default = "dev"
        |  description = "Deployment environment"
        |}""".stripMargin
    val file = HclParser.parse(hcl)
    file.blocks should have size 1
    val v = file.blocks.head
    v.blockType          shouldBe "variable"
    v.labels             shouldBe List("environment")
    v.body.attributes("default") shouldBe HclValue.Str("dev")
  }

  // ─── Multiple blocks ──────────────────────────────────────────────────────

  it should "parse multiple top-level blocks" in {
    val hcl =
      """resource "snowflake_database" "db1" { name = "DB1" }
        |resource "snowflake_database" "db2" { name = "DB2" }
        |resource "snowflake_warehouse" "wh" { name = "WH" }""".stripMargin
    val file = HclParser.parse(hcl)
    file.blocks should have size 3
  }

  // ─── Heredoc strings ──────────────────────────────────────────────────────

  it should "parse heredoc strings" in {
    val hcl =
      """resource "snowflake_view" "v" {
        |  statement = <<-SQL
        |    SELECT id, name
        |    FROM my_table
        |    WHERE active = TRUE
        |  SQL
        |}""".stripMargin
    val file  = HclParser.parse(hcl)
    val stmt  = HclValue.asString(file.blocks.head.body.attributes("statement"))
    stmt should include("SELECT id, name")
    stmt should include("WHERE active = TRUE")
  }
}
