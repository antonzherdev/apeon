package ru.apeon.sync.buffer

import ru.apeon.core.script._


class BufferParser(val decorated : ScriptParserComponent) extends ScriptParserDecorator{
  override def init(lexical: Lexer) {
    super.init(lexical)
    lexical.reserved += ("buffer")
  }

  def buffer : Parser[Buffer] = "buffer" ~> ident ~ ("{" ~> (objectStatement*) <~ "}") ^^ {
    case name ~ statements => Buffer(parser.module, parser.pack.get, name, statements)
  }

  override def statementDef = (buffer | super.statementDef)
}