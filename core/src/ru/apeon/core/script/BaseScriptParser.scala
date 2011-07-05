package ru.apeon.core.script

import ru.apeon.core.entity._

class BaseScriptParser(val parser : ScriptParserParser) extends ScriptParserComponent{
  def init(lexical: Lexer) {
    lexical.delimiters ++= List( "&&", "||",
      ">=", "<=", "=>", "==", "!=", "=", "(", ")", "{", "}", "``", "`", ".", ",", "<", ">", ":",
      "+=", "-=", "*=", "/=", "+", "-", "*", "/", "[", "]", "!", "->")
    lexical.reserved += (
            "def", "as", "to", "where", "by", "entity", "column", "primary", "default",
            "table", "discriminator", "one", "many", "query", "package", "datasource", "extends", "var", "val", "extend",
            "if", "else", "null", "import", "object", "join", "true", "false", "cached")
  }

  def statementDef : Parser[Statement] =
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
    | "{" ~> (statement*) <~ a("}", "in statement") ^^{case statements => Parentheses(statements)}
    )

  def valStatement : Parser[Val] =
    "val" ~> ident ~! (dataTypeSpec?) ~ ("=" ~> expression) ^^ {
      case name ~ dt ~ e => Val(name, e, dt)
    }
  def varStatement : Parser[Var] =
    "var" ~> ident ~! (dataTypeSpec?) ~ ("=" ~> expression) ^^ {
      case name ~ dt ~ e => Var(name, e, dt)
    }

  def defStatement : Parser[Def] =
    opt("cached") ~ ("def" ~> ident) ~! (defParameters?) ~ (dataTypeSpec?) ~ ("=" ~> statement) ^^ {
      case cached ~ name ~ parameters ~ result ~ statement =>
              Def(name, statement, parameters.getOrElse{Seq()}, result, cached.isDefined)
    }
  def defParameters : Parser[Seq[DefPar]] = "(" ~> repsep(defParameter, ",") <~ ")"
  def defParameter : Parser[DefPar] = ident ~ dataTypeSpec ^^ {case name ~ dataType => DefPar(name, dataType)}


  def dataType : Parser[ScriptDataType] = ident ^^ {
    case "Boolean" => ScriptDataTypeBoolean()
    case "Date" => ScriptDataTypeDate()
    case "Decimal" => ScriptDataTypeDecimal()
    case "Int" => ScriptDataTypeInteger()
    case "Integer" => ScriptDataTypeInteger()
    case "String" => ScriptDataTypeString()
    case "InputStream" => ScriptDataTypeInputStream()
    case entity => ScriptDataTypeEntityByName(entity)
  }
  def dataTypeSpec : Parser[ScriptDataType] = ":" ~> dataType

  def eqlConst : Parser[ConstEql] = eqlConstString ^^ (ConstEql(_))

  def eqlConstString: Parser[String] =
    elem("Eql constant", _.isInstanceOf[parser.lexical.EqlConstLit]) ^^ (_.chars)


  def builtInFunction : Parser[BuiltInFunction] = "{" ~> opt(repsep(ident, ",") <~ "=>") ~ (statement*) <~ a("}", "in builtin function") ^^ {
    case aliases ~ statements => BuiltInFunction(Parentheses(statements), aliases.getOrElse{Seq()})
  }

  def ref : Parser[Ref] = ident ~ (dataSourceRef?) ~ (parameters?) ~ (builtInFunction?) ^^ {
    case name ~ dataSource ~ parameters ~ Some(function) => Ref(name, Some(parameters.getOrElse(Seq()) :+ Par(function)), dataSource)
    case name ~ dataSource ~ parameters ~ None => Ref(name, parameters, dataSource)
  }
  def dataSourceRef : Parser[Expression] = "<" ~> (ref | string) <~ ">"

  def parameters : Parser[Seq[Par]] =  "(" ~> repsep(expression, ",") <~ ")" ^^ {case parameters => parameters.map{Par(_)}}

  def ifExpr : Parser[If] = ("if" ~> "(" ~> expression <~ ")") ~! statement ~ opt("else" ~> statement) ^^ {
    case e ~ t ~ f => If(e, t, f)
  }

  def not : Parser[Not] = "!" ~> expression ^^ {e => Not(e)}

  private var entityName = collection.mutable.Stack[String]()
  private var dataSourceName = ""

  def entity : Parser[Description] =
    "entity" ~> (ident ^^ {case name => {
      entityName.push(name)
      name
    }}) ~! (("<" ~> ident <~ ">") ^^ {case ds => {
      dataSourceName = ds
      ds
    }}) ~ (extendz?) ~ ("{" ~> (entityStatement*) <~ a("}", "in entity")) ^^ {
      case name ~ ds ~ ext ~ rows => {
        description(rows, ext)
      }
    }

  def description(statements: List[Any], extendEntityName: Option[String] = None): Description = {
    val ret = Description(parser.module, parser.pack.get,
      entityName.reverse.mkString("."), dataSourceName,
      statements.find(_.isInstanceOf[Table]).asInstanceOf[Option[Table]].getOrElse(Table("", entityName.reverse.mkString("_"))),
      statements.filter(_.isInstanceOf[DeclarationStatement]).asInstanceOf[Seq[DeclarationStatement]],
      statements.find(_.isInstanceOf[Discriminator]).getOrElse(DiscriminatorNull()).asInstanceOf[Discriminator],
      extendsEntityName = extendEntityName,
      declaredJoinedTables = statements.filter(_.isInstanceOf[JoinedTable]).asInstanceOf[Seq[JoinedTable]]
    )
    entityName.pop()
    ret
  }

  def extendz = "extends" ~> entityRef

  def entityRef = repsep(ident, ".") ^^ {case s => s.mkString(".")}

  def entityStatement : Parser[Any] = (attribute | one | manyBuiltIn | manyRef | discriminator | joinTable | objectStatement | tableStatement)

  def attribute : Parser[Attribute] =
    ("column" ~> ident) ~! (dbName?) ~ attributeDataType ~ primaryKey ~ (default?)  ^^ {
      case name ~ dbName ~ dataType ~ pk ~ default => {
        Attribute(parser.pack.get, name,
          toSources(dbName.getOrElse(Map("" -> FieldSource(name)))),
          dataType, isPrimaryKey = pk, default = default
        )
      }
    }

  def toSources(names: Map[String, FieldSource]): FieldSources = {
    FieldSources(names.getOrElse("", NullFieldSource()), names.filterNot(_._1.isEmpty))
  }

  def fieldSourcesToOne(name: String, dbNames: Option[Map[String, FieldSource]]): FieldSources = {
    toSources(dbNames.getOrElse(Map("" -> FieldSource(name + "_id"))))
  }

  def one : Parser[ToOne] =
    ("one" ~> ident) ~! (dbName?) ~ entityRef ~ primaryKey ~ (default?)  ^^ {
      case name ~ dbName ~ entity ~ pk ~ default => {
        ToOne(parser.pack.get, name,
          fieldSourcesToOne(name, dbName),
          entity, default, pk)
      }
    }

  def default : Parser[Default] = "default" ~>
          ( numericLit ^^ {case n => DefaultInt(n.toInt)}
                  | stringLit ^^ {case n => DefaultString(n)})

  def manyRef : Parser[ToManyRef] =
    ("many" ~> ident) ~ repsep(ident, ".")  ^^ {
      case name ~ column =>{
        if(column.size < 2) throw ParserException("Not set column or entity for to many %s".format(name), null)
        ToManyRef(parser.pack.get, name, column.take(column.size - 1).mkString(".") , column.last)
      }

    }

  private var builtInName = ""
  def manyBuiltIn : Parser[ToManyBuiltIn] =
    ("many" ~> (ident ^^ {case n => {
      builtInName = n
      n
    }})) ~ (dbName?) ~ (extendz?) ~ ("{" ^^ {case "{" => {
      entityName.push(builtInName)
      "{"
    }}) ~! ((entityStatement*) <~ a("}", "in builtin many"))  ^^ {
      case name ~ dbName ~ extendz ~ "{" ~ statements => {
        ToManyBuiltIn(parser.pack.get, name, description(
          statements.find{
            case s : ToOne => s.name == "parent"
            case _ => false
          } match {
          case None => ToOne(parser.pack.get, "parent",
            fieldSourcesToOne("parent", dbName),
            entityName.tail.reverse.mkString(".")
          ) +: statements
          case Some(p) => statements
        }, extendz
        ))
      }
    }

  def dbName : Parser[Map[String, FieldSource]] = "(" ~> dbName1 <~ ")"

  def dbName1 : Parser[Map[String, FieldSource]] = repsep(dbName2, ",") ^^ {
    case ns => ns.toMap
  }

  def dbName2 : Parser[(String, FieldSource)] =
    opt((ident | stringLit) <~ ".") ~ (ident | stringLit) ~ opt("<" ~> ident  <~ ">") ^^ {
      case table ~ column ~ dataSource => (dataSource.getOrElse(""), FieldSource(column, table))
    }

  def attributeDataType : Parser[AttributeDataType] = ident ~ opt(attributeDataTypePar) ^^ {
    case n ~ w => AttributeDataType(n, w.map(_._1), w.map(_._2))
  }
  def attributeDataTypePar : Parser[(Int, Int)]= "(" ~> numericLit ~ opt("," ~> numericLit) <~ ")" ^^ {
    case width ~ scale => (width.toInt, scale.getOrElse("0").toInt)
  }

  def primaryKey : Parser[Boolean] = opt("primary" ~> word("key")) ^^ {case v => v.isDefined}

  def table : Parser[Table] = opt(ident <~ ".") ~ repsep(ident, ".") ^^ {
    case schema ~ table => Table(schema.getOrElse(""), table.mkString("."))
  }

  def tableStatement : Parser[Table] = "table" ~> table

  def discriminator : Parser[Discriminator] = "discriminator" ~> dbName1 ~! ("=" ~>
          (stringLit|
                  numericLit ^^ {case num => num.toInt})) ^^ {
    case names ~ value => {
      DiscriminatorColumn(toSources(names), value)
    }
  }

  def joinTable: Parser[JoinedTable] = "join" ~> table ~! ("(" ~> ident <~ ")") ^^ {
    case table ~ column => JoinedTable(table, column)
  }

  def query : Parser[Query] = "query" ~> ident ~! ("{" ~> (statement*) <~ a("}", "for query")) ^^ {
    case name ~ statements => {
      val span = statements.span(_.isInstanceOf[Def])
      if(span._1.find{
        case Def("apply", _, Seq(), _, _) => true
        case _ => false}.isDefined)
      {
        Query(parser.model, parser.module, parser.pack.get, name, statements.filter(_.isInstanceOf[DeclarationStatement]).asInstanceOf[Seq[DeclarationStatement]] )
      }
      else {
        Query(parser.model, parser.module, parser.pack.get, name,
          Def("apply", span._2 match {
            case Seq(stm) => stm
            case _ => Parentheses(span._2)
          }) +: span._1.asInstanceOf[Seq[Def]]
        )
      }
    }
  }

  def objectDef : Parser[Object] = "object" ~> ident ~! ("{" ~> (objectStatement*) <~ a("}", "in object")) ^^ {
    case name ~ statements => Object(parser.module, parser.pack.get, name, statements)
  }

  def packDef : Parser[Package] = "package" ~> repsep(ident, ".") ^^ {
    case names  => {
      parser.pack = Some(Package(names.mkString(".")))
      parser.pack.get
    }
  }

  def datasource = "datasource" ~> ident ^^ {case name => DataSource(parser.pack.get, name)}

  def bracket = "(" ~> rep1sep(expression, ",") <~ ")" ^^ {
    case Seq(e) => e
    case s => Tuple(s)
  }

  def nullConst = "null" ^^^ {ConstNull()}
  def string = stringLit ^^ {case s => ConstString(s)}
  def numeric =
    (numericLit ~ opt("." ~> numericLit) ^^ {
      case s ~ None => ConstInt(s.toInt)
      case s ~ Some(d) => ConstDecimal(BigDecimal((s + "." + d).toDouble))
    } |
     "-" ~> numericLit ~ opt("." ~> numericLit) ^^ {
      case s ~ None => ConstInt(-s.toInt)
      case s ~ Some(d) => ConstDecimal(BigDecimal(-(s + "." + d).toDouble))
    })
  def minus = "-" ~> expression ^^ {
    case ConstInt(i) => ConstInt(-i)
    case ConstDecimal(i) => ConstDecimal(-i)
    case e => UMinus(e)
  }
  def seq = "[" ~> repsep(expression, ",") <~ "]" ^^ {s => ConstSeq(s)}
  def extendEntity = "extend" ~> "entity" ~> entityRef ~ ("{" ~> (extendEntityStatement*) <~ a("}", "in extend entity")) ^^ {
    case name ~ statements => ExtendEntity(parser.module, name, statements)
  }

  def extendEntityStatement = (attribute | one | manyBuiltIn | manyRef | defStatement)

  def importStm = "import" ~> repsep(ident, ".") ^^ {case n => Import(n.mkString("."))}

  def objectStatement : Parser[DeclarationStatement] =
    (defStatement
    | valStatement
    | varStatement
    )

  def term : Parser[Expression] = string | numeric | minus | eqlConst | nullConst |
          ifExpr | builtInFunction | bracket | seq | not |
          "true" ^^^ {ConstBoolean(true)} | "false" ^^^ {ConstBoolean(false)} | ref

  def expressionDef : Parser[Expression] = e500

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
            (("=" | "+=" | "-=" | "*=" | "/=" | "->") ~! expression)
  ) ^^ {
    case left ~ Some(r) => r._1 match {
      case "*" => Mul(left, r._2)
      case "/" => Div(left, r._2)
      case "=" => Set(left, r._2)
      case "+=" => SetPlus(left, r._2)
      case "-=" => SetMinus(left, r._2)
      case "*=" => SetMul(left, r._2)
      case "/=" => SetDiv(left, r._2)
      case "->" => MapItem(left, r._2)
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
            ("||" ~! e500) ^^ {case o ~ e => new ~(o, e) : Any}
          ) ^^ {
    case left ~ Some(r) =>  r match {
      case ~("||", right : Expression) => Or(left, right)
    }
    case left ~ None => left
  }
}

