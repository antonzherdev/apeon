package ru.apeon.core.script

import util.parsing.combinator.syntactical.StdTokenParsers
import util.parsing.combinator.lexical.StdLexical
import util.parsing.input.CharArrayReader.EofCh
import util.parsing.combinator.token.Tokens

object ScriptParser{
  def parse(model : ObjectModel, module : Module, code: String, fileName : Option[String] = None) : Script = {
    val data = new ScriptParserParser
    data._model = model
    data._module = module
    data._fileName = fileName
    data._component = new BaseScriptParser(data)
    data.parse(code)
  }

  def parse(model : ObjectModel, module : Module, pack : Package, code: String) : Script = {
    val data = new ScriptParserParser
    data._model = model
    data._module = module
    data._fileName = None
    data.pack = Some(pack)
    data._component = new BaseScriptParser(data)
    data.parse(code)
  }

  def parseStatement(model : ObjectModel, code: String) : Statement = {
    val data = new ScriptParserParser
    data._model = model
    data._module = CoreModule
    data._fileName = None
    data._component = new BaseScriptParser(data)
    data.parseStatement(code)
  }
}

final class ScriptParserParser extends StdTokenParsers with ApeonTokens
{
  private[script] var _model : ObjectModel = _
  private[script] var _module : Module =  CoreModule
  private[script] var _fileName : Option[String] = None
  var pack : Option[Package] = None

  private[script] var _component : ScriptParserComponent = _

  def model = _model
  def fileName = _fileName
  def component = _component
  def module = _module
  type Tokens = Lexer
  val lexical = new Tokens

  def parse(code : String) : Script = {
    component.init(lexical)

    val parser = phrase(script)
    val scanner = new lexical.Scanner(code)
    val ret = parser.apply(scanner)
    ret match {
      case Success(f, next) => f
      case NoSuccess(msg, next) => throw ParserException(msg)
    }
  }

  def parseStatement(code : String) : Statement = {
    component.init(lexical)

    val parser = phrase(statement)
    val scanner = new lexical.Scanner(code)
    val ret = parser.apply(scanner)
    ret match {
      case Success(f, next) => f
      case NoSuccess(msg, next) => throw ParserException(msg)
    }
  }

  def script : Parser[Script] = (statement*) ^^{case statements => {
    new Script(model, pack.get, statements, fileName)
  }}


  def statement : Parser[Statement] = ( component.statementDef.asInstanceOf[Parser[Statement]] | expression)

  def expression : Parser[Expression] = component.expressionDef.asInstanceOf[Parser[Expression]]

  def word(w : String) : Parser[String] =
    elem("identifier", { ch =>
      ch .isInstanceOf[lexical.Identifier] && ch.asInstanceOf[lexical.Identifier].chars == w
    }) ^^ (_.chars)
}

abstract class ScriptParserComponent {
  val parser : ScriptParserParser
  type Parser[T] = parser.Parser[T]

  def init(lexical : Lexer)

  def statementDef : Parser[Statement]
  def expressionDef : Parser[Expression]

  implicit def keyword(chars: String) = parser.keyword(chars)
  implicit def accept(e: parser.Elem) = parser.accept(e)

  def repsep[T](p: => Parser[T], q: => Parser[Any]): Parser[List[T]] =
    parser.repsep(p, q)

  def rep1sep[T](p : => Parser[T], q : => Parser[Any]): Parser[List[T]] =
    parser.rep1sep(p, q)

  def opt[T](p: => Parser[T]): Parser[Option[T]] =
    parser.opt(p)
  def ident: Parser[String] = parser.ident

  def word(w : String) : Parser[String] = parser.word(w)

  type ~[a, b] = parser.~[a, b]
  lazy val ~ = parser.~

  def elem(e: parser.Elem): Parser[parser.Elem] = parser.elem(e)
  def elem(kind: String, p: parser.Elem => Boolean) = parser.elem(kind, p)

  def numericLit: Parser[String] = parser.numericLit
  def stringLit: Parser[String] = parser.stringLit

  def expression = parser.component.expressionDef.asInstanceOf[Parser[Expression]]
  def statement =  (parser.component.statementDef.asInstanceOf[Parser[Statement]] | expression)

  def dataSourceRef : Parser[Expression]
  def ref : Parser[Ref]
  def eqlConstString: Parser[String]
}

abstract class ScriptParserDecorator extends ScriptParserComponent{
  val decorated : ScriptParserComponent
  val parser = decorated.parser

  def init(lexical: Lexer) {
    decorated.init(lexical)
  }
  def statementDef = decorated.statementDef.asInstanceOf[Parser[Statement]]
  def expressionDef = decorated.expressionDef.asInstanceOf[Parser[Expression]]

  def dataSourceRef = decorated.dataSourceRef.asInstanceOf[Parser[Expression]]
  def ref = decorated.ref.asInstanceOf[Parser[Ref]]
  def eqlConstString = decorated.eqlConstString.asInstanceOf[Parser[String]]
}


case class ParserException(msg : String) extends RuntimeException(msg)

class Lexer extends StdLexical with ApeonTokens {
  def mkString(chars: List[Char]): String = {
    val b = new StringBuilder
    var lastSymbol = '\0'
    for(char <- chars) {
      if(lastSymbol == '\\') {
        b.append(char match {
          case 'n' => '\n'
          case 't' => '\t'
          case s => s
        })
        lastSymbol = '\0'
      } else {
        if(char != '\\') {
          b.append(char)
        }
        lastSymbol = char
      }
    }
    b.result()
  }

  override def token =
    ( '`' ~ rep(chrExcept('`', EofCh)) ~ '`' ^^ {case '`' ~ chars ~ '`' => EqlConstLit(chars mkString "")}
    |'`' ~> failure("unclosed eql literal")
    | identChar ~ rep( identChar | digit )              ^^ { case first ~ rest => processIdent(first :: rest mkString "") }
    | digit ~ rep( digit )                              ^^ { case first ~ rest => NumericLit(first :: rest mkString "") }
    | '\'' ~ rep( str('\'') ) ~ '\'' ^^ { case '\'' ~ chars ~ '\'' => StringLit(mkString(chars)) }
    | '\"' ~ rep( str('\"') ) ~ '\"' ^^ { case '\"' ~ chars ~ '\"' => StringLit(mkString(chars)) }
    | EofCh                                             ^^^ EOF
    | '\'' ~> failure("unclosed string literal")
    | '\"' ~> failure("unclosed string literal")
    | delim
    | failure("illegal character")
    )

  override def whitespace: Parser[Any] = rep(
      whitespaceChar
    | '/' ~ '*' ~ rep( chrExceptPrev(('\0', EofCh), ('*', '/')) ) ~ '/'
    | '/' ~ '/' ~ rep( chrExcept(EofCh, '\n') )
    | '/' ~ '*' ~ failure("unclosed comment")
    )


  private var prevSymbol : Char = '\0'

  def chrExceptPrev(cs: (Char, Char)*) = elem("", ch => {
    val ret = cs forall (s => !((s._1 == '\0' || prevSymbol == s._1) && ch == s._2))
    prevSymbol = ch
    ret
  })

  def str(par : Char) = elem("String", ch => {
    val ret = if(prevSymbol != '\\') {
      ch != par && ch != EofCh
    } else true
    prevSymbol = ch
    ret
  })

}

trait ApeonTokens extends Tokens {
  case class EqlConstLit(chars : String) extends Token{
    override def toString = "`"+chars+"`"
  }
}

