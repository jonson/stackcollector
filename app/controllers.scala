package controllers

import play._
import play.mvc._
import util.Logging
import com.mongodb.casbah.Imports._
import collection.JavaConversions._
import collection.mutable.HashMap
import collection.mutable.HashSet
import scala.concurrent.ops.spawn
import model.{CrashLog, CrashReport}
import com.mongodb.casbah.MongoURI

object Mongo {
  val mongoUri = MongoURI("mongodb://localhost")
  val mongoConn = MongoConnection(mongoUri)
  val mongo = mongoConn.getDB("traces")

//  if (mongo.authenticate("test", "password")) {
//
//  }

  val crashlogs = new HashMap[String,MongoCollection]
  crashlogs += "prod" -> mongo("prod_logs")
  crashlogs += "release" -> mongo("release_logs")
  crashlogs += "dev" -> mongo("dev_logs")

  val crashreports = new HashMap[String,MongoCollection]
  crashreports += "prod" -> mongo("prod_reports")
  crashreports += "release" -> mongo("release_reports")
  crashreports += "dev" -> mongo("dev_reports")
}

object Application extends Controller with Logging {
    
  import views.Application._

  private val validModes = new HashSet[String]
  validModes += "prod"
  validModes += "release"
  validModes += "dev"
    
  def index (mode: String) = {

    // find the active stack traces
    val isvalid = validModes(mode);

    if (isvalid) {
      val traces = CrashReport.findCrashReports(mode)
      html.index (mode, traces);
    }
  }

  def trace(mode: String, id: String) = {

    val isvalid = validModes(mode);

    if (isvalid) {
      val trace = CrashReport.findCrashReport(mode, id)

      if (trace.isDefined) {
        html.trace(mode, trace.get)
      }
    }
  }

  def changeStatus(mode:String, id:String) = {
    val isvalid = validModes(mode);

    if (isvalid) {

      val status = request.params.get("status");
      log.debug("checking status")

      log.debug("mode is ok")

      if (status == "new" || status == "old") {

        log.debug("status is ok")

        val query = MongoDBObject("_id" -> new ObjectId(id))
        val inc = $set("status" -> status.toUpperCase)
        val obj = Mongo.crashreports(mode).findAndModify(query, inc)

        if (obj.isDefined) {
          // now get the new status
          val newStatus = obj.get.getAs[String]("status").get

          log.debug("new status=" + newStatus)

          crashreport(mode, id)
        }
      }

    }

  }

  def crashreport(mode: String, id: String) = {

    log.info("im here")
    val isvalid = validModes(mode);

    if (isvalid) {
      val crashReport = CrashReport.findCrashReport(mode, id)


      if (crashReport.isDefined) {

        val report = crashReport.get
        val buffer = new scala.collection.mutable.ListBuffer[CrashLog]

        report.instances.foreach(instance => {
          val logOpt = CrashReport.findCrashLog(mode, instance.toString)
          if (logOpt.isDefined) {
            buffer += logOpt.get
          }
        })

        log.info("crash report defined")
        html.crashreport(mode, crashReport.get, buffer.toList)
      } else {
        log.info("crash report not defined: " + id)
      }
    } else {
      log.info("invalid mode: " + mode)
    }
  }

  def findUniquePackages(mode: String) = {

    log.info("Find unique packages")

    val values = CrashReport.findUniquePackages(mode)

    log.info(values.toString)
    // do something here
  }

}

object Collector extends Controller with Logging {

  // maintain a list of fields we'll accept

  def androidProd = {
    android("prod")
  }

  def androidDev = {
    android("dev")
  }

  def androidRelease = {
    android("release")
  }

	def android (mode: String) = {

    log.debug("post request received")

    // strip out the body param, seems to be included
    val params = request.params.allSimple filter (param => !param._1.eq("body") && CrashReport.validField(param._1))

    // take a copy, not sure how safe the params object is in play
    val paramsCopy = collection.immutable.Map() ++ params

    // return as quickly as possible, don't make the calling thread wait for the mongo operation
    spawn {
      // there must be a better way to do this, we could use a thread pool, ...  actor??
      log.info("Starting processing")


      CrashReport.save(mode, paramsCopy)
      log.info("Processed")
    }

    log.info("Returning OK")
    Ok
	}
	
}