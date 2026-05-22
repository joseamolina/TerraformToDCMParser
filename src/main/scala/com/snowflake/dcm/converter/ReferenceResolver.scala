package com.snowflake.dcm.converter

import com.snowflake.dcm.hcl.{ExprPart, HclValue, LiteralPart, TemplatePart}
import com.snowflake.dcm.terraform.TerraformProject

/**
 * Resolves Terraform attribute-references and variable/local expressions to
 * concrete string values wherever possible.
 *
 * Resolution order:
 *   1. `var.name`        → variable default value
 *   2. `local.name`      → local value
 *   3. `resource_type.resource_name.attribute` → attribute of another resource
 *   4. Unresolvable       → Jinja2 template placeholder  {{ jinja_safe_name }}
 */
class ReferenceResolver(project: TerraformProject) {

  /**
   * Resolve an HclValue to a plain string.  Template strings are expanded;
   * unresolvable parts become Jinja2 placeholders.
   */
  def resolve(value: HclValue): String = value match {
    case HclValue.Str(s)           => s
    case HclValue.Num(n)           => if (n == n.toLong) n.toLong.toString else n.toString
    case HclValue.Bool(b)          => b.toString
    case HclValue.Null             => ""
    case HclValue.Lst(items)       => items.map(resolve).mkString(", ")
    case HclValue.Obj(_)           => ""
    case HclValue.Ref(path)        => resolveRef(path)
    case HclValue.TemplateStr(ps)  => ps.map(resolvePart).mkString
  }

  /** Resolve a dotted reference path to its string value. */
  def resolveRef(path: List[String]): String = path match {
    case "var" :: name :: Nil =>
      project.variables.get(name)
        .flatMap(_.default)
        .map(resolve)
        .getOrElse(s"{{ $name }}")

    case "var" :: name :: rest =>
      project.variables.get(name)
        .flatMap(_.default)
        .map(resolve)
        .getOrElse(s"{{ $name }}")

    case "local" :: name :: Nil =>
      project.locals.get(name)
        .map(l => resolve(l.value))
        .getOrElse(s"{{ $name }}")

    case "each" :: _ =>
      "{{ each_key }}"   // for_each loops - placeholder

    case rType :: rName :: "fully_qualified_name" :: Nil =>
      // Computed attribute — derive FQN from the resource's own attributes
      computeFqn(rType, rName)

    case rType :: rName :: attr :: Nil =>
      project.resources
        .getOrElse(rType, Map.empty)
        .get(rName)
        .flatMap(_.attr(attr))
        .map(resolve)
        .getOrElse(jinjaVar(path))

    case rType :: rName :: Nil =>
      project.resources
        .getOrElse(rType, Map.empty)
        .get(rName)
        .flatMap(_.attr("name"))
        .map(resolve)
        .getOrElse(jinjaVar(path))

    case _ =>
      jinjaVar(path)
  }

  // ─── fully_qualified_name computation ────────────────────────────────────

  /**
   * Compute the Snowflake fully-qualified name for a resource that uses the
   * Terraform computed attribute `fully_qualified_name`.
   *
   *   snowflake_database → NAME
   *   snowflake_schema   → DB.NAME
   *   snowflake_table    → DB.SCHEMA.NAME
   *   snowflake_view     → DB.SCHEMA.NAME
   *   anything else      → NAME
   */
  private def computeFqn(rType: String, rName: String): String = {
    val resources = project.resources.getOrElse(rType, Map.empty)
    resources.get(rName) match {
      case None => jinjaVar(List(rType, rName, "fully_qualified_name"))
      case Some(r) =>
        val name   = r.attr("name").map(resolve).getOrElse("")
        rType match {
          case "snowflake_schema" =>
            val db = r.attr("database").map(resolve).getOrElse("")
            if (db.nonEmpty) s"$db.$name" else name
          case "snowflake_table" | "snowflake_view" | "snowflake_dynamic_table" =>
            val db  = r.attr("database").map(resolve).getOrElse("")
            val sch = r.attr("schema").map(resolve).getOrElse("")
            (db, sch) match {
              case ("", "") => name
              case ("", s)  => s"$s.$name"
              case (d, "")  => s"$d.$name"
              case (d, s)   => s"$d.$s.$name"
            }
          case _ => name
        }
    }
  }

  // ─── Expression inside ${...} ─────────────────────────────────────────────

  private def resolvePart(part: TemplatePart): String = part match {
    case LiteralPart(t) => t
    case ExprPart(expr) => resolveExpr(expr)
  }

  /**
   * Parse a raw expression string like "var.environment" or
   * "snowflake_database.my_db.name" and resolve it.
   */
  private def resolveExpr(expr: String): String = {
    val path = expr.split("\\.").toList
    if (path.nonEmpty) resolveRef(path) else s"{{ ${toJinjaName(expr)} }}"
  }

  private def jinjaVar(path: List[String]): String = s"{{ ${toJinjaName(path.mkString("_"))} }}"

  private def toJinjaName(s: String): String =
    s.replaceAll("[^a-zA-Z0-9_]", "_")
}

object ReferenceResolver {
  def apply(project: TerraformProject): ReferenceResolver = new ReferenceResolver(project)
}
