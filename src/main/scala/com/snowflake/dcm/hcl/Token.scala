package com.snowflake.dcm.hcl

/** Parts that make up an interpolated string like "prefix_${var.env}_suffix" */
sealed trait TemplatePart
final case class LiteralPart(text: String) extends TemplatePart
final case class ExprPart(expr: String)    extends TemplatePart   // raw expression inside ${...}

/** HCL lexer tokens */
sealed trait Token
final case class TIdent(value: String)          extends Token  // identifier / keyword
final case class TString(value: String)          extends Token  // plain string literal
final case class TTemplateStr(parts: List[TemplatePart]) extends Token  // interpolated string
final case class TNumber(value: Double)          extends Token
final case class TBool(value: Boolean)           extends Token
case object TNull     extends Token
case object TLBrace   extends Token  // {
case object TRBrace   extends Token  // }
case object TLBracket extends Token  // [
case object TRBracket extends Token  // ]
case object TLParen   extends Token  // (
case object TRParen   extends Token  // )
case object TEq       extends Token  // =
case object TArrow    extends Token  // =>
case object TComma    extends Token  // ,
case object TDot      extends Token  // .
case object TEof      extends Token
