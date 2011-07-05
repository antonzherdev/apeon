package ru.apeon.core.loader

import java.io._
import java.nio.channels.FileChannel
import java.nio.charset.Charset
import akka.util.Logging
import ru.apeon.core.script._

object ScriptLoader extends Logging {
  def load(model : ObjectModel, modules : Seq[Module]) {
    val decorators = modules.map(_.parserDecorators).reduce(_ ++ _).map{className =>
      Loader.loadClass(className)
    }

    val files = modules/*.par*/.map{module => (module, new File(module.path + "/apeon"))}.
      filter(_._2.isDirectory).
      map(module => allFiles(module._2).map(file => (module._1, file))).
      foldLeft(Seq[(Module, File)]()){_ ++ _}

    val scripts = files/*.par*/.map{file =>
      log.info("Parsing file " + file._2.getAbsolutePath)
      val script = parse(model, file._1, file._2, decorators)
      log.info("Parsed file " + file._2.getAbsolutePath)
      script
    }

    scripts.foreach(_.evaluate())
    scripts.foreach(_.preFillRef())
    scripts.foreach(_.fillRef())
  }

  def allFiles(path : File) : Seq[File] = {
    val builder = Seq.newBuilder[File]
    for(file <- path.listFiles) {
      if(file.isDirectory) {
        builder ++= allFiles(file)
      }
      else if(file.getName.endsWith(".apeon")) {
        builder += file
      }
    }
    builder.result()
  }

  private def text(file : File) : String = {
    val stream = new FileInputStream(file)
    try {
      val fc = stream.getChannel
      val bb = fc.map(FileChannel.MapMode.READ_ONLY, 0, fc.size)

      Charset.forName("utf8").decode(bb).toString
    }
    finally {
      stream.close()
    }
  }

  def parse(model : ObjectModel, module : Module, file : File, decorators : Seq[Class[_]]) : Script = try {
    ScriptParser.parse(model, module, text(file), Some(file.getAbsolutePath), decorators)
  }
  catch {
    case pe : ParserException => throw ParserException("Parse error in file %s\n%s".format(file.getAbsolutePath, pe.msg), pe)
    case e : Throwable => throw ParserException("Parse error in file %s\n%s".format(file.getAbsolutePath, e.getMessage), e)
    case _ => throw ParserException("Unknown parse error in file %s".format(file.getAbsolutePath), null)
  }

}