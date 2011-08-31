package model

import _root_.controllers.Mongo
import collection.mutable.HashSet
import com.mongodb.casbah.Imports._
import collection.JavaConversions._
import com.google.gson.Gson
import util.Logging

object CrashLog extends Logging {

  private val validCrashFields = new HashSet[String]
  validCrashFields += "USER_CRASH_DATE"
  validCrashFields += "AVAILABLE_MEM_SIZE"
  validCrashFields += "STACK_TRACE"
  validCrashFields += "PHONE_MODEL"
  validCrashFields += "BRAND"
  validCrashFields += "ANDROID_VERSION"
  validCrashFields += "TOTAL_MEM_SIZE"
  validCrashFields += "DISPLAY"
  validCrashFields += "APP_VERSION_CODE"
  validCrashFields += "REPORT_ID"

  def validField (name : String) : Boolean = validCrashFields.contains(name)

  /**
   * Fetches all logs in the system
   */
  def find : List[CrashLog] = {
    // fetch the data (transform this to a List[CrashLog])
    val objs : Iterator[CrashLog] = for (obj <- Mongo.mongoColl.find()) yield {
      // json serialization would be better
      val cl = new CrashLog
      cl.stackTrace = obj.getAs[String]("STACK_TRACE")
      cl
    }

    return objs.toList
  }

  /**
   * Current # of logs in the system
   */
  def count : Long = {
    return Mongo.mongoColl.count
  }

  /**
   * Saves an object to the db
   */
  def save (params : Map[String,String]) : Unit = {

    // first look for an existing object w/ this stack trace, see if we can update it

    if (!params.isEmpty) {

      val stackTrace = params.get("STACK_TRACE")
      var updateObj : MongoDBObject = null;

      if (stackTrace.isDefined) {
        // do a query
        log.info("Checking for existing stack trace");
        val query = MongoDBObject ("STACK_TRACE" -> stackTrace)
        val existing = Mongo.mongoColl.findOne(query)

        // if existing one, up the count
        updateObj = existing.get

        // increment the count
        updateObj ++ $inc("count" -> 1)

        // todo: add a new date

      } else {
        log.info("New stack trace found")

        // create a new object
        val builder = MongoDBObject.newBuilder
        params foreach (param => {
            builder += param._1 -> param._2
        })

        builder += "count" -> 1
        updateObj = builder.result()
      }

      // seems weird, must be a way to set this
      updateObj += ("status" -> "new")

      Mongo.mongoColl.save(updateObj)
    } else {
      log.warn("bad request? did not find any valid fields")
    }
  }

}

class CrashLog {

  var stackTrace : Option[String] = None
  var androidVersion : Option[String] = None

}