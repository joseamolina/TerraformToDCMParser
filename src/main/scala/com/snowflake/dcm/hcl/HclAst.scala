package com.snowflake.dcm.hcl

/**
 * AST nodes produced by the HCL parser.
 *
 * HclValue models every right-hand-side value type supported in Terraform HCL:
 *   literals (string, number, bool, null), collections (list, object),
 *   interpolated template strings, and Terraform attribute references.
 */
sealed trait HclValue

object HclValue {
  final case class Str(value: String)                       extends HclValue
  final case class TemplateStr(parts: List[TemplatePart])   extends HclValue
  final case class Num(value: Double)                       extends HclValue
  final case class Bool(value: Boolean)                     extends HclValue
  case object Null                                          extends HclValue
  final case class Lst(items: List[HclValue])               extends HclValue
  final case class Obj(attrs: Map[String, HclValue])        extends HclValue
  /** A dotted attribute reference, e.g. snowflake_database.my_db.name → List("snowflake_database","my_db","name") */
  final case class Ref(path: List[String])                  extends HclValue

  /** Render a value back to a string representation (best-effort). */
  def asString(v: HclValue): String = v match {
    case Str(s)            => s
    case Num(n)            => if (n == n.toLong) n.toLong.toString else n.toString
    case Bool(b)           => b.toString
    case Ref(path)         => path.mkString(".")
    case TemplateStr(parts) =>
      parts.map {
        case LiteralPart(t) => t
        case ExprPart(e)    => s"$${$e}"
      }.mkString
    case Lst(items)        => items.map(asString).mkString(", ")
    case Obj(_)            => "<object>"
    case Null              => "null"
  }
}

/**
 * The body of an HCL block: a map of attribute assignments and a list of
 * nested blocks (in source order).
 */
final case class HclBody(
  attributes: Map[String, HclValue],
  blocks: List[HclBlock]
) {
  def attr(name: String): Option[HclValue]    = attributes.get(name)
  def strAttr(name: String): Option[String]   = attr(name).map(HclValue.asString)
  def block(name: String): Option[HclBlock]   = blocks.find(_.blockType == name)
  def allBlocks(name: String): List[HclBlock] = blocks.filter(_.blockType == name)
}

/** A single HCL block: type keyword + optional string labels + a body. */
final case class HclBlock(
  blockType: String,
  labels: List[String],
  body: HclBody
)

/** The top-level file: a sequence of blocks. */
final case class HclFile(blocks: List[HclBlock])
