package com.snowflake.dcm.hcl

import scala.collection.mutable

/**
 * Tokenises a Terraform HCL source file.
 *
 * Handles:
 *  - Line comments  (#, //)
 *  - Block comments (/* */)
 *  - Quoted strings with escape sequences and ${...} interpolation
 *  - Heredoc strings (<<MARKER and <<-MARKER)
 *  - Integer and floating-point numbers
 *  - Boolean and null literals
 *  - Identifiers (may include hyphens, per HCL spec)
 *  - All standard HCL punctuation
 */
class Lexer(input: String) {

  private var pos: Int     = 0
  private val len: Int     = input.length

  private def hasMore: Boolean             = pos < len
  private def current: Char                = if (hasMore) input(pos) else '\u0000'
  private def lookahead(n: Int = 1): Char  = if (pos + n < len) input(pos + n) else '\u0000'
  private def advance(): Char              = { val c = current; pos += 1; c }

  // ─── Public API ────────────────────────────────────────────────────────────

  def tokenize(): IndexedSeq[Token] = {
    val result = mutable.ArrayBuffer[Token]()
    while (hasMore) {
      skipWhitespaceAndComments()
      if (hasMore) readToken().foreach(result += _)
    }
    result += TEof
    result.toIndexedSeq
  }

  // ─── Whitespace / comment skipping ─────────────────────────────────────────

  private def skipWhitespaceAndComments(): Unit = {
    var continue = true
    while (continue && hasMore) {
      current match {
        case ' ' | '\t' | '\r' | '\n' =>
          advance()
        case '#' =>
          advance()
          while (hasMore && current != '\n') advance()
        case '/' if lookahead() == '/' =>
          advance(); advance()
          while (hasMore && current != '\n') advance()
        case '/' if lookahead() == '*' =>
          advance(); advance()
          while (hasMore && !(current == '*' && lookahead() == '/')) advance()
          if (hasMore) { advance(); advance() }  // consume */
        case _ =>
          continue = false
      }
    }
  }

  // ─── Token dispatch ─────────────────────────────────────────────────────────

  private def readToken(): Option[Token] = current match {
    case '{' => advance(); Some(TLBrace)
    case '}' => advance(); Some(TRBrace)
    case '[' => advance(); Some(TLBracket)
    case ']' => advance(); Some(TRBracket)
    case '(' => advance(); Some(TLParen)
    case ')' => advance(); Some(TRParen)
    case ',' => advance(); Some(TComma)
    case '.' => advance(); Some(TDot)
    case '=' =>
      advance()
      if (current == '>') { advance(); Some(TArrow) }
      else Some(TEq)
    case '"'  => Some(readString())
    case '<' if lookahead() == '<' => Some(readHeredoc())
    case c if c.isDigit => Some(readNumber())
    case '-' if lookahead().isDigit => Some(readNumber())
    case c if c.isLetter || c == '_' => Some(readIdentOrKeyword())
    case _ => advance(); None   // skip unknown character
  }

  // ─── String literal (with interpolation) ────────────────────────────────────

  private def readString(): Token = {
    advance() // opening "
    val parts  = mutable.ArrayBuffer[TemplatePart]()
    val litBuf = new StringBuilder

    while (hasMore && current != '"') {
      if (current == '\\') {
        advance()
        current match {
          case 'n'  => litBuf += '\n'; advance()
          case 'r'  => litBuf += '\r'; advance()
          case 't'  => litBuf += '\t'; advance()
          case '"'  => litBuf += '"';  advance()
          case '\'' => litBuf += '\''; advance()
          case '\\' => litBuf += '\\'; advance()
          case '$'  => litBuf += '$';  advance()   // escaped interpolation
          case 'u'  =>
            advance()
            val hex = (0 until 4).map { _ => val c = current; advance(); c }.mkString
            litBuf += Integer.parseInt(hex, 16).toChar
          case c    => litBuf += '\\'; litBuf += c; advance()
        }
      } else if (current == '$' && lookahead() == '{') {
        // Flush accumulated literal
        if (litBuf.nonEmpty) { parts += LiteralPart(litBuf.toString()); litBuf.clear() }
        advance(); advance() // skip ${
        val exprBuf = new StringBuilder
        var depth   = 1
        while (hasMore && depth > 0) {
          current match {
            case '{' => depth += 1; exprBuf += current; advance()
            case '}' =>
              depth -= 1
              if (depth > 0) { exprBuf += current; advance() }
              else advance()   // consume closing }
            case c   => exprBuf += c; advance()
          }
        }
        parts += ExprPart(exprBuf.toString().trim)
      } else {
        litBuf += current; advance()
      }
    }

    if (hasMore) advance()  // closing "
    if (litBuf.nonEmpty) parts += LiteralPart(litBuf.toString())

    buildStringToken(parts.toList)
  }

  private def buildStringToken(parts: List[TemplatePart]): Token = parts match {
    case Nil                                       => TString("")
    case List(LiteralPart(text))                   => TString(text)
    case _                                         => TTemplateStr(parts)
  }

  // ─── Heredoc ────────────────────────────────────────────────────────────────

  private def readHeredoc(): Token = {
    advance(); advance()  // skip <<
    val trim = current == '-'
    if (trim) advance()

    val markerBuf = new StringBuilder
    while (hasMore && current != '\n' && current != '\r') { markerBuf += current; advance() }
    if (hasMore && current == '\r') advance()
    if (hasMore && current == '\n') advance()

    val marker = markerBuf.toString().trim

    // Collect raw lines until the closing marker
    val rawContent = new StringBuilder
    var done = false
    while (hasMore && !done) {
      val lineBuf = new StringBuilder
      while (hasMore && current != '\n') { lineBuf += current; advance() }
      if (hasMore) advance()  // skip \n

      val lineStr  = lineBuf.toString()
      val trimLine = lineStr.replaceAll("^\\s+", "")  // ltrim

      if ((if (trim) trimLine else lineStr.trim) == marker) {
        done = true
      } else {
        rawContent ++= (if (trim) trimLine else lineStr)
        rawContent += '\n'
      }
    }

    // Strip trailing newline, then parse ${...} interpolations
    val content = rawContent.toString()
    val stripped = if (content.endsWith("\n")) content.dropRight(1) else content

    parseHeredocContent(stripped)
  }

  /**
   * Parse ${...} interpolations in a heredoc body.
   * Returns TTemplateStr if any interpolations are found, TString otherwise.
   */
  private def parseHeredocContent(content: String): Token = {
    val parts  = mutable.ArrayBuffer[TemplatePart]()
    val litBuf = new StringBuilder
    var i      = 0
    val n      = content.length

    while (i < n) {
      if (i + 1 < n && content(i) == '$' && content(i + 1) == '{') {
        if (litBuf.nonEmpty) { parts += LiteralPart(litBuf.toString()); litBuf.clear() }
        i += 2  // skip ${
        val exprBuf = new StringBuilder
        var depth   = 1
        while (i < n && depth > 0) {
          content(i) match {
            case '{' => depth += 1; exprBuf += content(i); i += 1
            case '}' =>
              depth -= 1
              if (depth > 0) { exprBuf += content(i) }
              i += 1
            case c   => exprBuf += c; i += 1
          }
        }
        parts += ExprPart(exprBuf.toString().trim)
      } else {
        litBuf += content(i); i += 1
      }
    }

    if (litBuf.nonEmpty) parts += LiteralPart(litBuf.toString())
    buildStringToken(parts.toList)
  }

  // ─── Number ─────────────────────────────────────────────────────────────────

  private def readNumber(): Token = {
    val start = pos
    if (current == '-') advance()
    while (hasMore && current.isDigit) advance()
    if (hasMore && current == '.' && lookahead().isDigit) {
      advance(); while (hasMore && current.isDigit) advance()
    }
    if (hasMore && (current == 'e' || current == 'E')) {
      advance()
      if (hasMore && (current == '+' || current == '-')) advance()
      while (hasMore && current.isDigit) advance()
    }
    TNumber(input.substring(start, pos).toDouble)
  }

  // ─── Identifier / keyword ────────────────────────────────────────────────────

  private def readIdentOrKeyword(): Token = {
    val start = pos
    while (hasMore && (current.isLetterOrDigit || current == '_' || current == '-')) advance()
    input.substring(start, pos) match {
      case "true"  => TBool(value = true)
      case "false" => TBool(value = false)
      case "null"  => TNull
      case word    => TIdent(word)
    }
  }
}

object Lexer {
  def tokenize(input: String): IndexedSeq[Token] = new Lexer(input).tokenize()
}
