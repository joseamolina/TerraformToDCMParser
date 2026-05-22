package com.snowflake.dcm.hcl

import scala.collection.mutable

/**
 * Recursive-descent parser for the subset of HCL used in Snowflake Terraform
 * configurations.
 *
 * Grammar (simplified):
 *
 *   file      ::= block*
 *   block     ::= IDENT string_label* '{' body '}'
 *   body      ::= (attribute | block)*
 *   attribute ::= IDENT '=' value
 *   value     ::= STRING | TEMPLATE_STRING | NUMBER | BOOL | NULL
 *               | list | object | reference | func_call
 *   list      ::= '[' (value (',' value)*)? ','? ']'
 *   object    ::= '{' (key ('='|'=>') value ','?)* '}'
 *   reference ::= IDENT ('.' IDENT)*
 *   func_call ::= IDENT '(' … ')'   → skipped, returns Null
 */
class HclParser(tokens: IndexedSeq[Token]) {

  private var pos: Int = 0

  private def peek(offset: Int = 0): Token =
    if (pos + offset < tokens.length) tokens(pos + offset) else TEof

  private def advance(): Token = { val t = peek(); pos += 1; t }

  private def consume(expected: Token): Unit = {
    val t = advance()
    if (t != expected) throw new ParseException(
      s"Expected $expected but got $t (pos $pos, context: ${tokens.slice(math.max(0, pos - 3), pos + 2).mkString(", ")})"
    )
  }

  // ─── Public entry point ───────────────────────────────────────────────────

  def parseFile(): HclFile = {
    val blocks = List.newBuilder[HclBlock]
    while (peek() != TEof) {
      tryParseBlock() match {
        case Some(b) => blocks += b
        case None    => advance()   // skip unrecognised top-level token
      }
    }
    HclFile(blocks.result())
  }

  // ─── Block ────────────────────────────────────────────────────────────────

  private def tryParseBlock(): Option[HclBlock] = peek() match {
    case TIdent(_) => Some(parseBlock())
    case _         => None
  }

  private def parseBlock(): HclBlock = {
    val blockType = advance() match {
      case TIdent(name) => name
      case t            => throw new ParseException(s"Expected block-type identifier, got $t")
    }

    // Collect string labels (zero or more quoted strings before '{')
    val labels = List.newBuilder[String]
    while (peek().isInstanceOf[TString]) {
      labels += advance().asInstanceOf[TString].value
    }

    consume(TLBrace)
    val body = parseBody()
    consume(TRBrace)

    HclBlock(blockType, labels.result(), body)
  }

  // ─── Body ─────────────────────────────────────────────────────────────────

  private def parseBody(): HclBody = {
    val attrs  = mutable.LinkedHashMap[String, HclValue]()
    val blocks = List.newBuilder[HclBlock]

    while (peek() != TRBrace && peek() != TEof) {
      peek() match {
        case TIdent(name) =>
          // Attribute:  ident = value
          // Block:      ident ("label")* { body }
          if (peek(1) == TEq) {
            advance()   // consume ident
            advance()   // consume =
            attrs(name) = parseValue()
          } else {
            blocks += parseBlock()
          }
        case _ =>
          advance()   // skip unexpected tokens
      }
    }

    HclBody(attrs.toMap, blocks.result())
  }

  // ─── Values ───────────────────────────────────────────────────────────────

  private def parseValue(): HclValue = peek() match {
    case TString(s)        => advance(); HclValue.Str(s)
    case TTemplateStr(ps)  => advance(); HclValue.TemplateStr(ps)
    case TNumber(n)        => advance(); HclValue.Num(n)
    case TBool(b)          => advance(); HclValue.Bool(b)
    case TNull             => advance(); HclValue.Null
    case TLBracket         => parseList()
    case TLBrace           => parseObjectValue()
    case TIdent(_) if peek(1) == TLParen => skipFunctionCall()
    case TIdent(_)         => parseReference()
    case _                 => advance(); HclValue.Null
  }

  private def parseList(): HclValue.Lst = {
    consume(TLBracket)
    val items = List.newBuilder[HclValue]
    while (peek() != TRBracket && peek() != TEof) {
      items += parseValue()
      if (peek() == TComma) advance()
    }
    consume(TRBracket)
    HclValue.Lst(items.result())
  }

  private def parseObjectValue(): HclValue.Obj = {
    consume(TLBrace)
    val attrs = mutable.LinkedHashMap[String, HclValue]()
    while (peek() != TRBrace && peek() != TEof) {
      val key = peek() match {
        case TIdent(k)  => advance(); k
        case TString(k) => advance(); k
        case t          => advance(); t.toString
      }
      if (peek() == TEq || peek() == TArrow) advance()
      val value = parseValue()
      attrs(key) = value
      if (peek() == TComma) advance()
    }
    consume(TRBrace)
    HclValue.Obj(attrs.toMap)
  }

  /** Parse a dotted reference like  snowflake_database.my_db.name */
  private def parseReference(): HclValue.Ref = {
    val path = List.newBuilder[String]
    path += advance().asInstanceOf[TIdent].value
    while (peek() == TDot) {
      peek(1) match {
        case TIdent(_) =>
          advance()   // consume dot
          path += advance().asInstanceOf[TIdent].value
        case TNumber(n) =>
          // Terraform index access like tuple.0.attr — treat index as string segment
          advance()
          path += n.toInt.toString
        case _ =>
          // Something else after the dot; stop here
          return HclValue.Ref(path.result())
      }
    }
    HclValue.Ref(path.result())
  }

  /**
   * Skip a function call expression like toset([...]) and return Null.
   * We don't need to evaluate functions for migration purposes.
   */
  private def skipFunctionCall(): HclValue.Null.type = {
    advance()   // function name
    advance()   // (
    var depth = 1
    while (peek() != TEof && depth > 0) {
      peek() match {
        case TLParen => depth += 1; advance()
        case TRParen => depth -= 1; advance()
        case _       => advance()
      }
    }
    HclValue.Null
  }
}

class ParseException(msg: String) extends RuntimeException(msg)

object HclParser {
  def parse(input: String): HclFile = {
    val tokens = Lexer.tokenize(input)
    new HclParser(tokens).parseFile()
  }
}
