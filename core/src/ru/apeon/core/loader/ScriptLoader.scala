package ru.apeon.core.loader

import java.io._
import java.nio.channels.FileChannel
import java.nio.charset.Charset
import akka.util.Logging
import ru.apeon.core.script._

object ScriptLoader extends Logging {
//  def load() {
//    load(EntityConfiguration.model)
//  }
//  def load(model : ObjectModel) {
//    load((new InitialContext).lookup("java:comp/env/apeonDeployPath").asInstanceOf[String].split(';').toSeq, model)
//  }

  def load(model : ObjectModel, dirs : Seq[File]) {
    val files = dirs.map(dir => allFiles(dir)).foldLeft(Seq[File]()){_ ++ _}

    val scripts = files.map{file =>
      log.info("Parsing file " + file.getAbsolutePath)
      val script = parse(model, file)
      log.info("Parsed file " + file.getAbsolutePath)
      script
    }

    val env = new DefaultEnvironment(model)
    scripts.foreach(_.evaluate(env))
    scripts.foreach(_.preFillRef)
    scripts.foreach(_.fillRef(env))
  }

  def allFiles(path : File) : Seq[File] = {
    val builder = Seq.newBuilder[File]
    for(file <- path.listFiles) {
      if(file.isDirectory) {
        builder ++= allFiles(path)
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

  def parse(model : ObjectModel, file : File) : Script = try {
    ScriptParser.parse(model, text(file))
  }
  catch {
    case pe : ParserException => throw ParserException("Parse error in file %s\n%s".format(file.getAbsolutePath, pe.msg))
    case e : Throwable => throw new RuntimeException("Parse error in file %s\n%s".format(file.getAbsolutePath, e.getMessage), e)
    case _ => throw ParserException("Unknown parse error in file %s".format(file.getAbsolutePath))
  }

}