package ru.apeon.core.eql

import util.parsing.combinator.syntactical.StdTokenParsers
import util.parsing.combinator.Parsers
import java.lang.String
import ru.apeon.core.entity._
import util.parsing.combinator.token.Tokens
import util.parsing.combinator.lexical.StdLexical
import util.parsing.input.CharArrayReader.EofCh
import ru.apeon.core.script.{Imports, ObjectModel}
import collection.mutable.Buffer

/**
 * @author Anton Zherdev
 */

object EqlParser {
  def apply(s : String,
            model : ObjectModel = EntityConfiguration.model,
            imports : Option[Imports] = None,
            externalCreator : Option[(String) => External] = None): Statement =
  {
    new EqlParser(model, imports, externalCreator).parse(s)
  }

  def parseExpression(s : String,
                      model : ObjectModel = EntityConfiguration.model,
                      imports : Option[Imports] = None,
                      externalCreator : Option[(String) => External] = None) : Expression = {
    new EqlParser(model, imports, externalCreator).parseExpression(s)
  }

  def parseSelect(s : String,
                  model : ObjectModel = EntityConfiguration.model,
                  imports : Option[Imports] = None,
                  externalCreator : Option[(String) => External] = None) : Select = {
    new EqlParser(model, imports, externalCreator).parseSelect(s)
  }
}

class EqlParser(val model : ObjectModel,
             val imports : Option[Imports],
             val externalCreator : Option[(String) => External] = None)
        extends StdTokenParsers with Parsers with EqlTokens
{
  type Tokens = Lexer
  val lexical = new Tokens

  lexical.delimiters ++= List(">=", ">", "<=", "<", "!=", ".", ",", "=", ":", "(", ")", "!", "<", ">")
  lexical.reserved += ("select", "from", "as", "where", "delete", "like", "and", "or", "null", "exists")

  def query : Parser[Statement] = (select | delete)

  def select : Parser[Select] = opt(selectPart) ~ ("from" ~> from) ~ opt(where) ^^ {
    case s ~ f ~ w => Select(f, s.getOrElse(Seq()), w)
  }

  def delete : Parser[Delete] = "delete" ~> "from" ~> ident ~ opt(where) ^^ {
    case e ~ w => Delete(FromEntity(model.entityDescription(e, imports), None), w)
  }

  def selectPart : Parser[Seq[Column]] = "select" ~> repsep(column, ",")

  def as : Parser[String] = "as" ~> ident

  def column : Parser[Column] = exp ~ opt(as) ^^ {
    case e ~ Some(n) => new Column(e, n)
    case e ~ None => e match {
      case r : Ref => new Column(e, r.column)
      case _ => throw new EqlParserError("Unknown column name")
    }
  }

  def from : Parser[From] = rep1sep(ident, ".") ~ (dataSource?) ~ (alias?)  ^^ {
    case refs ~ dataSource ~ alias =>
      val entityName = Buffer[String]()
      var entity : Option[Description] = None
      for(part <- refs) {
        if(entity.isEmpty) {
          entityName.append(part)
          entity = model.entityDescriptionOption(entityName.mkString("."), imports)
        }
        else {
          entity = Some(entity.get.field(part).asInstanceOf[ToMany].entity)
        }
      }
      entity match {
        case Some(e) => FromEntity(e, alias, dataSource.getOrElse(DataSourceExpressionDefault()))
        case None => FromToMany(ref(refs), alias)
      }

  }

  def dataSource : Parser[DataSourceExpression] = "<" ~> ident <~ ">" ^^ {
    case s => DataSourceExpressionDataSource(model.dataSource(s, imports))
  }

  def alias : Parser[String] = "as" ~> ident

  def where : Parser[Expression] = "where" ~> exp

  def term : Parser[Expression] =
    (external | brackets | numericConstant | stringConstant | parameter |
            functionCall | selectExpression | nullTerm | exists | not | columnRef)

  def brackets : Parser[Expression] = "(" ~> exp <~ ")"

  def nullTerm : Parser[Expression] = "null" ^^^ {ConstNull()}

  def functionCall : Parser[FunctionCall] = ident ~ ("(" ~> repsep(exp, ",") <~ ")") ^^ {
    case name ~ parameters => name match {
      case "sum" => Sum(parameters.head)
      case _ => SqlFunctionCall(name, parameters)
    }
  }

  def exists : Parser[Exists] = "exists" ~> "(" ~> columnRef ~ (as?) ~ (where?) <~ ")" ^^ {
    case ref ~ as ~ where => Exists(FromToMany(ref, as), where)
  }

  def not : Parser[Not] = "!" ~> exp ^^ {case e => Not(e)}

  def selectExpression : Parser[ESelect] = "(" ~> "select" ~> exp ~ ("from" ~> from) ~ opt(where) <~ ")" ^^ {
    case select ~ from ~ where => ESelect(select, from, where)
  }

  def exp : Parser[Expression] = (binary(minPrec) | term)

  def parameter : Parser[Parameter] = ":" ~> ident ^^ {case n => Parameter(n)}

  def numericConstant : Parser[ConstNumeric] = numericLit ^^ {
    case n : String => new ConstNumeric(BigDecimal(n))
  }

  def stringConstant : Parser[ConstString] = stringLit ^^ {
    case s => ConstString(s)
  }

  def ref(refs: List[String]): Ref = {
    refs match {
      case Seq(ref) => Ref(None, ref)
      case _ => {
        val i = refs.iterator
        var ref = Ref(i.next(), i.next())
        while (i.hasNext)
          ref = Ref(ref, i.next())
        ref
      }
    }
  }

  def columnRef : Parser[Ref] = rep1sep(ident,".") ^^ {
    case refs => ref(refs)
  }


  def binaryOp(level:Int):Parser[((Expression,Expression)=>Expression)] = {
    level match {
      case 3 =>
        "=" ^^^ { (a:Expression, b:Expression) => new Equal(a, b) } |
        "like" ^^^ {(a:Expression, b:Expression) => new Like(a, b)} |
        "!=" ^^^ {(a:Expression, b:Expression) => new NotEqual(a, b)} |
        ">" ^^^ {(a:Expression, b:Expression) => new More(a, b)} |
        ">=" ^^^ {(a:Expression, b:Expression) => new MoreOrEqual(a, b)} |
        "<" ^^^ {(a:Expression, b:Expression) => new Less(a, b)} |
        "<=" ^^^ {(a:Expression, b:Expression) => new LessOrEqual(a, b)}
      case 2 =>
        "and" ^^^ { (a:Expression, b:Expression) => new And(a, b) }
      case 1 =>
        "or" ^^^ {(a:Expression, b:Expression) => new Or(a, b)}
      case _ => throw new EqlParserError("bad precedence level "+level)
    }
  }
  val minPrec = 1
  val maxPrec = 3
  def binary(level : Int) : Parser[Expression] =
    if (level > maxPrec) term
    else binary(level + 1) * binaryOp(level)


  def external: Parser[External] =
    elem("External", _.isInstanceOf[ExternalLit]) ^^ {
      case s => externalCreator.get(s.chars)
    }

  def parseOption(s:String) : Option[Statement] = {
    phrase(query)(new lexical.Scanner(s)) match {
      case Success(tree, _) => Some(tree)
      case e: NoSuccess => None
    }
  }

  def parseSelect(s : String) : Select = {
    phrase(select)(new lexical.Scanner(s)) match {
      case Success(tree, _) => tree
      case e: NoSuccess =>
        throw new EqlParserError("Bad syntax: "+s)
    }
  }

  def parseExpression(s : String) : Expression = {
    phrase(exp)(new lexical.Scanner(s)) match {
      case Success(tree, _) => tree
      case e: NoSuccess =>
        throw new EqlParserError("Bad syntax: "+s)
    }
  }

  def parse(s : String): Statement = {
    phrase(query)(new lexical.Scanner(s)) match {
      case Success(tree, _) => tree
      case e: NoSuccess =>
        throw new EqlParserError("Bad syntax: "+s)
    }
  }
}

class EqlParserError(var s : String) extends Exception(s)

class Lexer extends StdLexical with EqlTokens {
  override def token : Parser[Token] =
    ( '%' ~ rep(chrExcept('%', EofCh)) ~ '%' ^^ {case '%' ~ chars ~ '%' => ExternalLit(chars mkString "")}
    |'%' ~> failure("unclosed eql literal")
    | super.token
    )
}

trait EqlTokens extends Tokens {
  case class ExternalLit(chars : String) extends Token{
    override def toString = "%"+chars+"%"
  }
}
