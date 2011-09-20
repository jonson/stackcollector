package controllers

import play._
import play.mvc._
import util.Logging
import com.mongodb.casbah.Imports._
import collection.JavaConversions._
import collection.mutable.HashSet
import model.CrashLog
import scala.concurrent.ops.spawn
import collection.immutable.HashMap

object Mongo {
  val mongoConn = MongoConnection()
  val mongo = mongoConn("local_test")
  val mongoColl = mongo("test_data")
}

object Application extends Controller with Logging {
    
  import views.Application._
    
  def index = {
    // find the active stack traces

    val traces = CrashLog.find

    html.index (traces);

  }

  def trace( id: String) = {


    val trace = CrashLog.find(id)

    if (trace.isDefined) {
      html.trace(trace.get)
    }


  }

}

object Collector extends Controller with Logging {

  // maintain a list of fields we'll accept

	def android = {

    log.debug("post request received")

    // strip out the body param, seems to be included
//    val params = request.params.allSimple filter (param => !param._1.eq("body") && CrashLog.validField(param._1))

    // temp disable valid params check
    val params = request.params.allSimple filter (param => !param._1.eq("body"))

    // take a copy, not sure how safe the params object is in play
    val paramsCopy = collection.immutable.Map() ++ params

    // return as quickly as possible, don't make the calling thread wait for the mongo operation
    spawn {
      // there must be a better way to do this, we could use a thread pool, ...  actor??
      log.info("Starting processing")
      CrashLog.save(paramsCopy)
      log.info("Processed")
    }

    log.info("Returning OK")
    Ok
	}
	
}