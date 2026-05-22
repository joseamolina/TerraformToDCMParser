# main.tf — infraestructura de Snowflake gestionada con Terraform
terraform {
  required_providers {
    snowflake = {
      source  = "Snowflake-Labs/snowflake"
      version = "~> 0.87"
    }
  }
}

resource "snowflake_database" "analytics" {
  name    = "ANALYTICS_DB"
  comment = "Base de datos de analítica"
}

resource "snowflake_schema" "sales" {
  database = snowflake_database.analytics.name
  name     = "SALES"
}

resource "snowflake_table" "orders" {
  database = snowflake_database.analytics.name
  schema   = snowflake_schema.sales.name
  name     = "ORDERS"
  column { name = "id"     type = "NUMBER(38,0)" nullable = false }
  column { name = "amount" type = "FLOAT" }
  column { name = "date"   type = "DATE" }
}

resource "snowflake_warehouse" "analytics_wh" {
  name           = "ANALYTICS_WH"
  warehouse_size = "XSMALL"
  auto_suspend   = 60
  auto_resume    = true
}

resource "snowflake_role" "analyst" {
  name = "ANALYST_ROLE"
}

resource "snowflake_grant_privileges_to_role" "analyst_db" {
  privileges = ["USAGE"]
  role_name  = snowflake_role.analyst.name
  on_account_object {
    object_type = "DATABASE"
    object_name = snowflake_database.analytics.name
  }
}