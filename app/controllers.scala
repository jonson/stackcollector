package controllers

import play._
import play.mvc._
import util.Logging
import com.mongodb.casbah.Imports._
import collection.JavaConversions._
import collection.mutable.HashMap
import collection.mutable.HashSet
import model.CrashLog
import scala.concurrent.ops.spawn

object Mongo {
  val mongoConn = MongoConnection()
  val mongo = mongoConn("traces")
  val mongoColl = new HashMap[String,MongoCollection]
  mongoColl += "prod" -> mongo("prod")
  mongoColl += "release" -> mongo("release")
  mongoColl += "dev" -> mongo("dev")

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
      val traces = CrashLog.find(mode)
      html.index (mode, traces);
    }
  }

  def trace(mode: String, id: String) = {

    val isvalid = validModes(mode);

    if (isvalid) {
      val trace = CrashLog.find(mode, id)

      if (trace.isDefined) {
        html.trace(mode, trace.get)
      }
    }

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
    val params = request.params.allSimple filter (param => !param._1.eq("body") && CrashLog.validField(param._1))

    // temp disable valid params check


    // take a copy, not sure how safe the params object is in play
    val paramsCopy = collection.immutable.Map() ++ params

    // return as quickly as possible, don't make the calling thread wait for the mongo operation
    spawn {
      // there must be a better way to do this, we could use a thread pool, ...  actor??
      log.info("Starting processing")
      CrashLog.save(mode, paramsCopy)
      log.info("Processed")
    }

    log.info("Returning OK")
    Ok
	}
	
}