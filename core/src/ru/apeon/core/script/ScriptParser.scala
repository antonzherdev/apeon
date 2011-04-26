package ru.apeon.core.script

import util.parsing.combinator.syntactical.StdTokenParsers
import ru.apeon.core.entity._
import util.parsing.combinator.lexical.StdLexical
import util.parsing.input.CharArrayReader.EofCh
import util.parsing.combinator.token.Tokens

object ScriptParser{
  def parse(pack : Package, code: String) : Script = (new ScriptParser(pack.model, Some(pack))).parse(code)
  def parse(model : ObjectModel, code: String) : Script = (new ScriptParser(model, None)).parse(code)
}

class ScriptParser(model : ObjectModel, var pack : Option[Package] = None) extends StdTokenParsers with ApeonTokens{
  type Tokens = Lexer
  val lexical = new Tokens

  def parse(code : String) = {
    val parser = phrase(script)
    val scanner = new lexical.Scanner(code)
    val ret = parser.apply(scanner)
    ret match {
      case Success(f, next) => f
      case NoSuccess(msg, next) => throw ParserException(msg)
    }
  }

  def parseStatement(code : String) : Statement = {
    val parser = phrase(statement)
    val scanner = new lexical.Scanner(code)
    val ret = parser.apply(scanner)
    ret match {
      case Success(f, next) => f
      case NoSuccess(msg, next) => throw ParserException(msg)
    }
  }

  lexical.delimiters ++= List( "&&", "||",
    ">=", "<=", "=>", "==", "!=", "=", "(", ")", "{", "}", "``", "`", ".", ",", "<", ">", ":",
    "+=", "-=", "*=", "/=", "+", "-", "*", "/")
  lexical.reserved += (
          "def", "sync", "as", "to", "where", "by", "entity", "column", "primary", "key", "default",
          "table", "discriminator", "one", "many", "query", "package", "datasource", "extends", "var", "val", "extend",
          "if", "else", "null", "import", "object")

  def script : Parser[Script] = (statement*) ^^{case statements => {
    new Script(pack.getOrElse(statements.find(_.isInstanceOf[Package])).asInstanceOf[Package], statements)
  }}


  def statement : Parser[Statement] =
    ( defStatement
    | valStatement
    | varStatement
    | importStm
    | entity
    | extendEntity
    | query
    | objectDef
    | packDef
    | datasource
    | "{" ~> (statement*) <~ "}" ^^{case statements => Parentheses(statements)}
    | syncDef
    | expression
    )

  def expression : Parser[Expression] = e500

  def term : Parser[Expression] = string | int | ref | eqlConst | nullConst | ifExpr | builtInFunction

  def e200 : Parser[Expression] = term ~ opt("." ~> ref ~ opt("." ~> repsep(ref, "."))) ^^{
    case e ~ Some(r) => {
      var dot = Dot(e, r._1)
      r._2.foreach(_.foreach({ref => dot = Dot(dot, ref)}))
      dot
    }
    case e ~ None => e
  }

  def e300 : Parser[Expression] = e200 ~ opt(
    (("*" | "/") ~! e300) |
            (("=" | "+=" | "-=" | "*=" | "/=") ~! expression)
  ) ^^ {
    case left ~ Some(r) => r._1 match {
      case "*" => Mul(left, r._2)
      case "/" => Div(left, r._2)
      case "=" => Set(left, r._2)
      case "+=" => SetPlus(left, r._2)
      case "-=" => SetMinus(left, r._2)
      case "*=" => SetMul(left, r._2)
      case "/=" => SetDiv(left, r._2)
    }
    case left ~ None => left
  }
  def e400 : Parser[Expression] = e300 ~ opt(("+" | "-") ~! e400) ^^ {
    case left ~ Some(r) => r._1 match {
      case "+" => Plus(left, r._2)
      case "-" => Minus(left, r._2)
    }
    case left ~ None => left
  }
  def e450 : Parser[Expression] = e400 ~
          opt((("==" | "!=" | ">=" | "<=") ~! e400 )  |
                  (("<" | ">") ~ e400))  ^^
          {
            case left ~ Some(r) => r._1 match {
              case "==" => Equal(left, r._2)
              case "!=" => NotEqual(left, r._2)
              case ">=" => MoreOrEqual(left, r._2)
              case ">" => More(left, r._2)
              case "<=" => LessOrEqual(left, r._2)
              case "<" => Less(left, r._2)
            }
            case left ~ None => left
          }

  def e470 : Parser[Expression] = e450 ~
          opt("&&" ~! e470) ^^ {
    case left ~ Some(r) =>  r match {
      case ~("&&", right : Expression) => And(left, right)
    }
    case left ~ None => left
  }

  def e500 : Parser[Expression] = e470 ~
          opt(
            ("||" ~! e500) ^^ {case o ~ e => new ~(o, e) : Any} |
                    ("sync" ~> "by" ~! expression) |
                    syncWith
          ) ^^ {
    case left ~ Some(r) =>  r match {
      case ~("||", right : Expression) => Or(left, right)
      case ~("by", by : Expression) => SyncBy(left, by)
      case right : SyncRight => SyncEntity(left, right.sourceAlias, right.destination, right.where, right.statements)
    }
    case left ~ None => left
  }

  case class SyncRight(sourceAlias : String, destination : SyncRef, where : String, statements : Seq[Statement])
  def syncWith : Parser[SyncRight] =
    ("sync" ~> (as?)) ~ ("to" ~> ident) ~! (dataSourceRef?) ~ (as?) ~ ("where" ~> eqlConstString) ~ opt("{" ~> (statement*) <~ "}") ^^ {
      case sourceAs ~ destination ~ dataSource ~ destinationAs ~ where ~ statements =>
        SyncRight(sourceAs.getOrElse{"source"},
          SyncRef(destination, destinationAs.getOrElse{"dest"}, dataSource),
          where,
          statements.getOrElse{Seq()})
    }


  def valStatement : Parser[Val] =
    "val" ~> ident ~! (dataTypeSpec?) ~ ("=" ~> expression) ^^ {
      case name ~ dt ~ e => Val(name, e, dt)
    }
  def varStatement : Parser[Var] =
    "var" ~> ident ~! (dataTypeSpec?) ~ ("=" ~> expression) ^^ {
      case name ~ dt ~ e => Var(name, e, dt)
    }

  def defStatement : Parser[Def] =
    ("def" ~> ident) ~! (defParameters?) ~ (dataTypeSpec?) ~ ("=" ~> statement) ^^
            {case name ~ parameters ~ result ~ statement => Def(name, statement, parameters.getOrElse{Seq()}, result)}
  def defParameters : Parser[Seq[DefPar]] = "(" ~> repsep(defParameter, ",") <~ ")"
  def defParameter : Parser[DefPar] = ident ~ dataTypeSpec ^^ {case name ~ dataType => DefPar(name, dataType)}


  def dataType : Parser[ScriptDataType] = ident ^^ {
    case "boolean" => ScriptDataTypeBoolean()
    case "date" => ScriptDataTypeDate()
    case "decimal" => ScriptDataTypeDecimal()
    case "int" => ScriptDataTypeInteger()
    case "integer" => ScriptDataTypeInteger()
    case "string" => ScriptDataTypeString()
    case entity => ScriptDataTypeEntityByName(entity)
  }
  def dataTypeSpec : Parser[ScriptDataType] = ":" ~> dataType

  def syncDef : Parser[SyncDeclaration] =
    ("sync"~> ident) ~ (as?) ~ ("to" ~> ident) ~ (dataSourceRef?) ~ (as?) ~ ("where" ~> eqlConstString) ~ opt("{" ~> (statement*) <~ "}") ^^ {
      case source ~ sourceAs ~ destination ~ dataSource ~ destinationAs ~ where ~ statements =>
        SyncDeclaration(
          SyncRef(source, sourceAs.getOrElse{"source"}),
          SyncRef(destination, destinationAs.getOrElse{"dest"}, dataSource),
          where,
          statements.getOrElse{Seq()}
        )
    }


  def as : Parser[String] = "as" ~> ident

  def eqlConst : Parser[ConstEql] = eqlConstString ^^ (ConstEql(_))

  def eqlConstString: Parser[String] =
    elem("Eql constant", _.isInstanceOf[EqlConstLit]) ^^ (_.chars)


  def builtInFunction : Parser[BuiltInFunction] = "{" ~> opt(repsep(ident, ",") <~ "=>") ~! (statement*) <~ "}" ^^ {
    case aliases ~ statements => BuiltInFunction(statements match {
      case Seq(one) => one
      case _ => Parentheses(statements)
    }, aliases.getOrElse{Seq()})
  }

  def ref : Parser[Ref] = ident ~ (dataSourceRef?) ~ (parameters?) ~ (builtInFunction?) ^^ {
    case name ~ dataSource ~ parameters ~ Some(function) => Ref(name, Some(parameters.getOrElse(Seq()) :+ Par(function)), dataSource)
    case name ~ dataSource ~ parameters ~ None => Ref(name, parameters, dataSource)
  }
  def dataSourceRef : Parser[Expression] = "<" ~> expression <~ ">"

  def parameters : Parser[Seq[Par]] =  "(" ~> repsep(expression, ",") <~ ")" ^^ {case parameters => parameters.map{Par(_)}}

  def ifExpr : Parser[If] = ("if" ~> "(" ~> expression <~ ")") ~! statement ~ opt("else" ~> statement) ^^ {
    case e ~ t ~ f => If(e, t, f)
  }

  def entity : Parser[Description] =
    "entity" ~> ident ~ opt("(" ~> table <~ ")") ~! (_extends?) ~ ("{" ~> (entityStatement*) <~ "}") ^^ {
      case name ~ table ~ ext ~ rows => Description(pack.get,
        name, table.getOrElse(Table("", name)).asInstanceOf[Table],
        rows.filter(_.isInstanceOf[DeclarationStatement]).asInstanceOf[Seq[DeclarationStatement]],
        rows.find(_.isInstanceOf[Discriminator]).getOrElse(DiscriminatorNull()).asInstanceOf[Discriminator],
        extendsEntityName = ext,
        declaredJoinedTables = rows.filter(_.isInstanceOf[JoinedTable]).asInstanceOf[Seq[JoinedTable]]
      )
    }

  def _extends = "extends" ~> entityRef

  def entityRef = ident

  def entityStatement : Parser[Any] = (attribute | one | many | discriminator | joinTable | objectStatement)

  def attribute : Parser[Attribute] =
    ("column" ~> ident) ~! (dbName?) ~ attributeDataType ~ primaryKey ~ (default?)  ^^ {
      case name ~ dbName ~ dataType ~ pk ~ default => {
        val names = dbName.getOrElse(Map())
        Attribute(pack.get, name,
          FieldSources(names.getOrElse("", FieldSource(name)), names.filterNot(_._1.isEmpty)),
          dataType, isPrimaryKey = pk, default = default
        )
      }
    }

  def one : Parser[ToOne] =
    ("one" ~> ident) ~! (dbName?) ~ entityRef ~ (default?)  ^^ {
      case name ~ dbName ~ entity ~ default => {
        val names = dbName.getOrElse(Map())
        ToOne(pack.get, name,
          FieldSources(names.getOrElse("", FieldSource(name + "_id")), names.filterNot(_._1.isEmpty)),
          entity, default = default)
      }
    }

  def default : Parser[Default] = "default" ~>
          ( numericLit ^^ {case n => DefaultInt(n.toInt)}
                  | stringLit ^^ {case n => DefaultString(n)})

  def many : Parser[ToMany] =
    ("many" ~> ident) ~! entityRef ~ ("(" ~> ident <~ ")")  ^^ {
      case name ~ entity ~ column =>
        ToMany(pack.get, name, entity, column)
    }

  def dbName : Parser[Map[String, FieldSource]] =
    ("(" ~> repsep(dbName1, ",") <~ ")") ^^ {
      case ns => ns.toMap
    }

  def dbName1 : Parser[(String, FieldSource)] =
    opt((ident | stringLit) <~ ".") ~ (ident | stringLit) ~ opt("<" ~> ident  <~ ">") ^^ {
      case table ~ column ~ dataSource => (dataSource.getOrElse(""), FieldSource(column, table))
    }

  def attributeDataType : Parser[AttributeDataType] = ident ~ opt(attributeDataTypePar) ^^ {
    case n ~ w => AttributeDataType(n, w.getOrElse((0, 0))._1, w.getOrElse((0, 0))._2)
  }
  def attributeDataTypePar : Parser[(Int, Int)]= "(" ~> numericLit ~ opt("," ~> numericLit) <~ ")" ^^ {
    case width ~ scale => (width.toInt, scale.getOrElse("0").toInt)
  }

  def primaryKey : Parser[Boolean] = opt("primary" ~> "key") ^^ {case v => v.isDefined}

  def table : Parser[Table] = opt(ident <~ ".") ~ ident ^^ {case schema ~ table => Table(schema.getOrElse(""), table)}
  def discriminator : Parser[Discriminator] = "discriminator" ~> ident ~! ("=" ~> stringLit) ^^ {
    case column ~ value => DiscriminatorColumn(column, value)
  }

  def joinTable: Parser[JoinedTable] = "table" ~> table ~! ("(" ~> ident <~ ")") ^^ {
    case table ~ column => JoinedTable(table, column)
  }

  def query : Parser[Query] = "query" ~> ident ~! ("{" ~> (statement*) <~ "}") ^^ {
    case name ~ statements => {
      val span = statements.span(_.isInstanceOf[Def])
      if(span._1.find{
        case Def("apply", _, Seq(), _) => true
        case _ => false}.isDefined)
      {
        Query(pack.get, name, statements.filter(_.isInstanceOf[DeclarationStatement]).asInstanceOf[Seq[DeclarationStatement]] )
      }
      else {
        Query(pack.get, name,
          Def("apply", span._2 match {
            case Seq(stm) => stm
            case _ => Parentheses(span._2)
          }) +: span._1.asInstanceOf[Seq[Def]]
        )
      }
    }
  }

  def objectDef : Parser[Object] = "object" ~> ident ~! ("{" ~> (objectStatement*) <~ "}") ^^ {
    case name ~ statements => Object(pack.get, name, statements)
  }

  def packDef : Parser[NonRootPackage] = "package" ~> repsep(ident, ".") ^^ {
    case names  => Package(model, names).asInstanceOf[NonRootPackage]
  }

  def datasource = "datasource" ~> ident ^^ {case name => DataSource(pack.get, name)}

  def nullConst = "null" ^^^ {ConstNull()}
  def string = stringLit ^^ {case s => ConstString(s)}
  def int =
    (numericLit ^^ {case s => ConstInt(s.toInt)} |
     "-" ~> numericLit ^^ {case s => ConstInt(- s.toInt)})

  def extendEntity = "extend" ~> "entity" ~> ident ~ ("{" ~> (extendEntityStatement*) <~ "}") ^^ {
    case name ~ statements => ExtendEntity(name, statements)
  }

  def extendEntityStatement = (attribute | one | many)

  def importStm = "import" ~> repsep(ident, ".") ^^ {case n => Import(n.mkString("."))}

  def objectStatement : Parser[DeclarationStatement] =
    (defStatement
    | valStatement
    | varStatement
    )
}

case class ParserException(msg : String) extends RuntimeException(msg)

class Lexer extends StdLexical with ApeonTokens {
  override def token =
    ( '`' ~ rep(chrExcept('`', EofCh)) ~ '`' ^^ {case '`' ~ chars ~ '`' => EqlConstLit(chars mkString "")}
    |'`' ~> failure("unclosed eql literal")
    | super.token
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
}

trait ApeonTokens extends Tokens {
  case class EqlConstLit(chars : String) extends Token{
    override def toString = "`"+chars+"`"
  }
}

