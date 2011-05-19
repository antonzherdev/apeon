package ru.apeon.schedule

import org.quartz.impl.StdSchedulerFactory
import org.quartz._
import javax.naming.InitialContext
import ru.apeon.core.entity._
import akka.util.Logging
import ru.apeon.core.loader.Listener

class QuartzListener extends Listener{
  var scheduler : Scheduler = null

  def preLoad() {

  }

  def load() {
    scheduler = StdSchedulerFactory.getDefaultScheduler
    scheduler.start()

    val con = new InitialContext
    try {
      con.lookup("java:comp/env/apeonSchedules").asInstanceOf[String].split(';').toSeq.foreach{scheduleName =>
        val description = con.lookup("java:comp/env/" + scheduleName).asInstanceOf[String]
        val arr = description.split("=>")
        val trigger = new CronTrigger(scheduleName, null, arr(0))

        arr(1).split(';').toSeq.map(_.trim).foreach{queryName =>
          val job = new JobDetail(scheduleName + "-" + queryName, classOf[QueryJob])
          job.getJobDataMap.put("queryName", queryName)
          scheduler.scheduleJob(job, trigger);
        }
      }
    }
    catch {
      case e : Throwable => {}
    }
    //    val trigger = TriggerUtils.makeMinutelyTrigger("Test")
  }

  def unload() {
    scheduler.shutdown()
  }
}

class QueryJob extends Job  with Logging  {
  def execute(p: JobExecutionContext) = {
    val name = p.getJobDetail.getJobDataMap.getString("queryName")
    log.info("Schedule " + name)
    try {
      EntityConfiguration.model.obj(name).asInstanceOf[Query].execute()
    }
    catch {
      case e : Throwable => e.printStackTrace()
    }
  }
}