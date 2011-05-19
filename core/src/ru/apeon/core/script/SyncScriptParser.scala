package ru.apeon.core.script

class SyncScriptParser(val decorated : ScriptParserComponent) extends ScriptParserDecorator {

  override def statementDef = (super.statementDef | syncDef)

  override def init(lexical: Lexer) {
    super.init(lexical)
    lexical.reserved += "sync"
  }

  case class SyncRight(sourceAlias : String, destination : SyncRef, where : String, statements : Seq[Statement])
  def syncWith : Parser[SyncRight] =
    ("sync" ~> (as?)) ~ ("to" ~> repsep(ident, ".")) ~! (dataSourceRef?) ~ (as?) ~ ("where" ~> eqlConstString) ~ opt("{" ~> (statement*) <~ "}") ^^ {
      case sourceAs ~ destination ~ dataSource ~ destinationAs ~ where ~ statements =>
        SyncRight(sourceAs.getOrElse{"source"},
          SyncRef(destination.mkString("."), destinationAs.getOrElse{"dest"}, dataSource),
          where,
          statements.getOrElse{Seq()})
    }

  def syncDef : Parser[SyncDeclaration] =
    ("sync"~> repsep(ident, ".")) ~ (as?) ~ ("to" ~> repsep(ident, ".")) ~ (dataSourceRef?) ~ (as?) ~ ("where" ~> eqlConstString) ~ opt("{" ~> (statement*) <~ "}") ^^ {
      case source ~ sourceAs ~ destination ~ dataSource ~ destinationAs ~ where ~ statements =>
        SyncDeclaration(
          SyncRef(source.mkString("."), sourceAs.getOrElse{"source"}),
          SyncRef(destination.mkString("."), destinationAs.getOrElse{"dest"}, dataSource),
          where,
          statements.getOrElse{Seq()}
        )
    }


  def as : Parser[String] = "as" ~> ident

  override def expressionDef = super.expressionDef ~
          opt(
            ("sync" ~> "by" ~! expression) |
                    syncWith
          ) ^^ {
    case left ~ Some(r) =>  r match {
      case ~("by", by : Expression) => SyncBy(left, by)
      case right : SyncRight => SyncEntity(left, right.sourceAlias, right.destination, right.where, right.statements)
    }
    case left ~ None => left
  }
}
