package ru.apeon.core.script

import ru.apeon.core.entity._
import javax.naming.InitialContext
import java.io._
import java.nio.channels.FileChannel
import java.nio.charset.Charset
import akka.util.Logging
import akka.dispatch.{Futures}

object ScriptLoader extends Logging {
  def load() {
    load(EntityConfiguration.model)
  }
  def load(model : ObjectModel) {
    load((new InitialContext).lookup("java:comp/env/apeonDeployPath").asInstanceOf[String].split(';').toSeq, model)
  }

  def load(path : Seq[String], model : ObjectModel) {
    var packFiles = Seq[File]()
    path.foreach(p => packFiles = packFiles ++ load(p, model))

    val scripts = packFiles.map{packFile =>
      Futures.future(300000) {
        log.info("Parsing package file " + packFile.getAbsolutePath)
        val packScript = parse(model, packFile)
        log.info("Parsed package file " + packFile.getAbsolutePath)
        val pack =  packScript.pack
        val files = packFile.getParentFile.listFiles.toSeq.filter{file =>
          file.getName.endsWith(".apeon") && file != packFile
        }
        val futures = files.map{file =>
          Futures.future(60000) {
            log.info("Parsing file " + file.getAbsolutePath)
            val script = parse(pack, file)
            log.info("Parsed file " + file.getAbsolutePath)
            script
          }
        }

        val ret = packScript +: futures.map{future =>
          future.await.resultOrException.get
        }
        log.info("End parsing package " + pack.fullName)
        ret
      }
    }.map{_.await.resultOrException.get}.foldLeft(Seq() : Seq[Script]){(scripts, result)=>
      result ++ scripts
    }
    val env = new DefaultEnvironment(model)
    scripts.foreach(_.evaluate(env))
    scripts.foreach(_.preFillRef)
    scripts.foreach(_.fillRef(env))
  }

  def load(path : String, model : ObjectModel)  : Seq[File] = {
    var files = Seq[File]()
    val pathFile = new File(path)
    if(!pathFile.exists) throw new RuntimeException("Folder \"%s\" not exists".format(path))
    pathFile.listFiles.foreach{file =>
      if(file.isDirectory)
        files = files ++ loadDir(file, model)
    }
    files
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
  }

  def parse(pack : Package, file : File) : Script = try {
    ScriptParser.parse(pack, text(file))
  }
  catch {
    case pe : ParserException => throw ParserException("Parse error in file %s\n%s".format(file.getAbsolutePath, pe.msg))
    case e : Throwable => throw new RuntimeException("Parse error in file %s\n%s".format(file.getAbsolutePath, e.getMessage), e)
    case _ => throw ParserException("Unknown parse error in file %s".format(file.getAbsolutePath))
  }

  private def loadDir(dir : File, model : ObjectModel = EntityConfiguration.model ) : Seq[File] = {
    val files = dir.listFiles.toSeq
    files.map{file =>
      if(file.isDirectory)
        loadDir(file, model)
      else
        Seq()
    }.foldLeft(Seq[File]()){_ ++ _} ++ files.find{_.getName == "package.apeon"}
  }
}