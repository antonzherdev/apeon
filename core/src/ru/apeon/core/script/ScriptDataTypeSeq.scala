package ru.apeon.core.script


abstract class ScriptDataTypeCollection extends ScriptDataType{
  def dataType : ScriptDataType
}

case class ScriptDataTypeSeq(dataType : ScriptDataType) extends ScriptDataTypeCollection

case class ScriptDataTypeMap(keyDataType : ScriptDataType, valueDataType : ScriptDataType) extends ScriptDataTypeCollection {
  val dataType = ScriptDataTypeMapItem(keyDataType, valueDataType)
}

case class ScriptDataTypeMapItem(keyDataType : ScriptDataType, valueDataType : ScriptDataType) extends ScriptDataType {
  override lazy val declarations : Seq[Declaration] = Seq(
    new Declaration {
      def name = "key"
      def dataType(env: Environment, parameters: Option[Seq[Par]]) = keyDataType
      def value(env: Environment, parameters: Option[Seq[ParVal]], dataSource: Option[Expression]) =
        env.ref.asInstanceOf[(_, _)]._1
    },
    new Declaration {
      def name = "value"
      def dataType(env: Environment, parameters: Option[Seq[Par]]) = valueDataType
      def value(env: Environment, parameters: Option[Seq[ParVal]], dataSource: Option[Expression]) =
        env.ref.asInstanceOf[(_, _)]._2
    }
  )
}

object ScriptDataTypeSeqDescription {
  def iterable = Seq(foreach, filter, filterNot, find, isEmpty, size, groupBy, mapFunc, toMap, head, headOption, last, lastOption, tail, HashCodeDeclaration)
  def map = iterable ++ Seq(mapGet, mapApply)
  def seq = iterable ++ Seq(seqApply)

  def t(env : Environment) = env.dotType.get.asInstanceOf[ScriptDataTypeCollection]
  def tp(env : Environment) = env.dotType.get.asInstanceOf[ScriptDataTypeCollection].dataType

  abstract class OneBuiltInDeclaration extends Declaration {
    override def builtInParameters(env: Environment, parameters: Option[Seq[Par]], parameterNumber: Int, parameter: Par) =
      Seq(BuiltInParameterDef("_", tp(env)))
    def value(env: Environment, parameters: Option[Seq[ParVal]], dataSource: Option[Expression]) = {
      value(env,  env.ref.asInstanceOf[Iterable[Any]], parameters.get.head.value.asInstanceOf[BuiltInFunction])
    }
    def value(env : Environment, items : Iterable[Any], f : BuiltInFunction) : Any
    override def parameters = Seq(DefPar("f", ScriptDataTypeBuiltInFunction()))
  }

  val foreach = new OneBuiltInDeclaration{
    def name = "foreach"
    def dataType(env: Environment, parameters: Option[Seq[Par]]) = ScriptDataTypeUnit()
    def value(env: Environment, items: Iterable[Any], f: BuiltInFunction) {
      items.foreach {
        item => f.run(env, item)
      }
    }
  }

  val filter = new OneBuiltInDeclaration{
    def name = "filter"
    def dataType(env: Environment, parameters: Option[Seq[Par]]) = t(env)
    def value(env: Environment, items: Iterable[Any], f: BuiltInFunction) = items.filter{item =>
      f.run(env, item).asInstanceOf[Boolean]}
  }

  val filterNot = new OneBuiltInDeclaration{
    def name = "filterNot"
    def dataType(env: Environment, parameters: Option[Seq[Par]]) = t(env)
    def value(env: Environment, items: Iterable[Any], f: BuiltInFunction) = items.filterNot{item =>
      f.run(env, item).asInstanceOf[Boolean]}
  }


  val find = new OneBuiltInDeclaration{
    def name = "find"
    def dataType(env: Environment, parameters: Option[Seq[Par]]) =
      ScriptDataTypeOption(tp(env))
    def value(env: Environment, items: Iterable[Any], f: BuiltInFunction) = items.find {
      item => f.run(env, item).asInstanceOf[Boolean]
    }
  }

  val isEmpty = new Declaration {
    def value(env: Environment, parameters: Option[Seq[ParVal]], dataSource: Option[Expression]) =
      env.ref.asInstanceOf[Traversable[_]].isEmpty
    def name = "isEmpty"
    def dataType(env: Environment, parameters: Option[Seq[Par]]) = ScriptDataTypeBoolean()
  }

  val size = new Declaration {
    def value(env: Environment, parameters: Option[Seq[ParVal]], dataSource: Option[Expression]) =
      env.ref.asInstanceOf[Traversable[_]].size
    def name = "size"
    def dataType(env: Environment, parameters: Option[Seq[Par]]) = ScriptDataTypeInteger()
  }

  val groupBy = new OneBuiltInDeclaration {
    def name = "groupBy"
    def dataType(env: Environment, parameters: Option[Seq[Par]]) =
      ScriptDataTypeMap(parameters.get.head.expression.evaluate(env).asInstanceOf[BuiltInFunction].statement.dataType(env), t(env))
    def value(env: Environment, items: Iterable[Any], f: BuiltInFunction) =
      items.groupBy(item => f.run(env, item))
  }

  val mapFunc = new OneBuiltInDeclaration {
    def name = "map"
    def dataType(env: Environment, parameters: Option[Seq[Par]]) =
      ScriptDataTypeSeq(parameters.get.head.expression.evaluate(env).asInstanceOf[BuiltInFunction].statement.dataType(env))
    def value(env: Environment, items: Iterable[Any], f: BuiltInFunction) =
      items.map(item => f.run(env, item))
  }

  val toMap = new Declaration {
    def name = "toMap"
    def dataType(env: Environment, parameters: Option[Seq[Par]]) = env.dotType match {
      case Some(ScriptDataTypeSeq(item : ScriptDataTypeMapItem)) => ScriptDataTypeMap(item.keyDataType, item.valueDataType)
      case s => throw ScriptException("Unsupported collection data type %s".format(s))
    }
    def value(env: Environment, parameters: Option[Seq[ParVal]], dataSource: Option[Expression]) =
      env.ref.asInstanceOf[Traversable[(_, _)]].toMap
  }

  val mapGet = new Declaration {
    def name = "get"
    def dataType(env: Environment, parameters: Option[Seq[Par]]) =
      ScriptDataTypeOption(env.dotType.get.asInstanceOf[ScriptDataTypeMap].valueDataType)
    def value(env: Environment, parameters: Option[Seq[ParVal]], dataSource: Option[Expression]) =
      env.ref.asInstanceOf[Map[Any, Any]].get(parameters.get.head.value)
    override def correspond(env: Environment, parameters: Option[Seq[Par]]) = parameters match {
      case Some(Seq(Par(_, _))) => true
      case _ => false
    }
  }
  val mapApply = new Declaration {
    def name = "apply"
    def dataType(env: Environment, parameters: Option[Seq[Par]]) =
      env.dotType.get.asInstanceOf[ScriptDataTypeMap].valueDataType
    def value(env: Environment, parameters: Option[Seq[ParVal]], dataSource: Option[Expression]) =
      env.ref.asInstanceOf[Map[Any, Any]].apply(parameters.get.head.value)
    override def correspond(env: Environment, parameters: Option[Seq[Par]]) = parameters match {
      case Some(Seq(Par(_, _))) => true
      case _ => false
    }
  }
  val seqApply = new Declaration {
    def name = "apply"
    def dataType(env: Environment, parameters: Option[Seq[Par]]) =
      env.dotType.get.asInstanceOf[ScriptDataTypeSeq].dataType
    def value(env: Environment, parameters: Option[Seq[ParVal]], dataSource: Option[Expression]) =
      env.ref.asInstanceOf[Seq[Any]].apply(parameters.get.head.value.asInstanceOf[Int])
    override def parameters = Seq(DefPar("index", ScriptDataTypeInteger()))
  }
  val head = new Declaration {
    def name = "head"
    def dataType(env: Environment, parameters: Option[Seq[Par]]) = tp(env)
    def value(env: Environment, parameters: Option[Seq[ParVal]], dataSource: Option[Expression]) =
      env.ref.asInstanceOf[Traversable[Any]].head
  }
  val last = new Declaration {
    def name = "last"
    def dataType(env: Environment, parameters: Option[Seq[Par]]) = tp(env)
    def value(env: Environment, parameters: Option[Seq[ParVal]], dataSource: Option[Expression]) =
      env.ref.asInstanceOf[Traversable[Any]].last
  }
  val headOption = new Declaration {
    def name = "headOption"
    def dataType(env: Environment, parameters: Option[Seq[Par]]) = ScriptDataTypeOption(tp(env))
    def value(env: Environment, parameters: Option[Seq[ParVal]], dataSource: Option[Expression]) =
      env.ref.asInstanceOf[Traversable[Any]].headOption
  }
  val lastOption = new Declaration {
    def name = "lastOption"
    def dataType(env: Environment, parameters: Option[Seq[Par]]) = ScriptDataTypeOption(tp(env))
    def value(env: Environment, parameters: Option[Seq[ParVal]], dataSource: Option[Expression]) =
      env.ref.asInstanceOf[Traversable[Any]].lastOption
  }
  val tail = new Declaration {
    def name = "tail"
    def dataType(env: Environment, parameters: Option[Seq[Par]]) = t(env)
    def value(env: Environment, parameters: Option[Seq[ParVal]], dataSource: Option[Expression]) =
      env.ref.asInstanceOf[Traversable[Any]].tail
  }
}
