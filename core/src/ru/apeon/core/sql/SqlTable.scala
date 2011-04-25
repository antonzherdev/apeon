package ru.apeon.core.sql


case class SqlTable(schema : String, name : String) {
  override def toString = schema match {
    case "" => "\"%s\"".format(name)
    case _ =>  "\"%s\".\"%s\"".format(schema, name)
}
}

case class SqlDataType(name : String, width : Option[Int] = None, scale : Option[Int] = None) {
  override def toString = ((width, scale)) match {
    case (Some(w), Some(s)) => "%s(%d, %d)".format(name, w, s)
    case (Some(w), _) => "%s(%d)".format(name, w)
    case _ => name
  }

  def isEqualsTypeName(tp : String) =
    if(tp == "text" || tp == "long varchar")
      name == "text" || name == "long varchar"
    else if(tp == "int" || tp == "integer")
      name == "int" || name == "integer"
    else
      name == tp

  override def equals(obj: Any) = obj match {
    case tp : SqlDataType =>  isEqualsTypeName(tp.name) && width == tp.width && scale == tp.scale
    case _ => false
  }
}

case class SqlColumn(table : SqlTable, name : String, dataType : SqlDataType, nulls : Boolean = true, default : Option[String] = None)

abstract class SqlForeignKeyAction()
case class SqlForeignKeyActionRestrict() extends SqlForeignKeyAction {
  override def toString = "restrict"
}
case class SqlForeignKeyActionSetNull() extends SqlForeignKeyAction {
  override def toString = "set null"
}
case class SqlForeignKeyActionSetDefault() extends SqlForeignKeyAction {
  override def toString = "set default"
}
case class SqlForeignKeyActionCascade() extends SqlForeignKeyAction {
  override def toString = "cascade"
}

case class SqlReference(table : SqlTable, columns : Seq[String]) {
  def column = columns.mkString(", ")

  def columnQuote = columns.map(col => "\"%s\"".format(col)).mkString(", ")
}

case class SqlForeignKey(name : String, primary : SqlReference, foreign : SqlReference,
                         onUpdate : SqlForeignKeyAction, onDelete : SqlForeignKeyAction)