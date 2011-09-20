package model

import _root_.controllers.Mongo
import collection.mutable.HashSet
import com.mongodb.casbah.Imports._
import collection.JavaConversions._
import com.google.gson.Gson
import util.Logging
import org.joda.time.DateTime
import org.joda.time.format.ISODateTimeFormat

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
  validCrashFields += "CUSTOM_DATA"
  validCrashFields += "APP_VERSION_NAME"

  def validField (name : String) : Boolean = validCrashFields.contains(name)

  /**
   * Fetches all logs in the system
   */
  def find (mode: String) : List[CrashLog] = {
    // fetch the data (transform this to a List[CrashLog])
    val objs : Iterator[CrashLog] = for (obj <- Mongo.mongoColl(mode).find().sort(MongoDBObject("lastDate" -> -1))) yield {
      // json serialization would be better
      val cl = new CrashLog
      cl.stackTrace = obj.getAs[String]("STACK_TRACE")
      cl.date = obj.getAs[DateTime]("USER_CRASH_DATE")
      cl.id = obj.getAs[ObjectId]("_id")
      cl.count = obj.getAs[Int]("count")
      cl.lastDate = obj.getAs[DateTime]("lastDate")
      cl
    }

    return objs.toList
  }

  def find (mode: String, id : String ) : Option[CrashLog] = {
    val query = MongoDBObject("_id" -> new ObjectId(id))
    val obj = Mongo.mongoColl(mode).findOne(query)
    if (obj.isDefined) {
      val cl = new CrashLog
      cl.androidVersion = obj.get.getAs[String]("ANDROID_VERSION")
      cl.stackTrace = obj.get.getAs[String]("STACK_TRACE")
      cl.model = obj.get.getAs[String]("PHONE_MODEL")
      cl.display = obj.get.getAs[String]("DISPLAY")
      cl.date = obj.get.getAs[DateTime]("USER_CRASH_DATE")
      cl.id = obj.get.getAs[ObjectId]("_id")
      cl.count = obj.get.getAs[Int]("count")
      cl.lastDate = obj.get.getAs[DateTime]("lastDate")
      cl.developerMode = obj.get.getAs[String]("DEVELOPER_MODE")
      cl.mode = obj.get.getAs[String]("MODE")

      // TODO: missing APP_VERSION_CODE from ACRA

      return Option(cl)
    }

    return None
  }

  /**
   * Current # of logs in the system
   */
  def count(mode : String) : Long = {
    return Mongo.mongoColl(mode).count
  }

  /**
   * Saves an object to the db
   */
  def save (mode: String, params : Map[String,String]) : Unit = {

    // first look for an existing object w/ this stack trace, see if we can update it

    if (!params.isEmpty) {

      val stackTrace = params.get("STACK_TRACE")

      if (stackTrace.isDefined) {
        // do a query
        log.info("Checking for existing stack trace");
        val query = MongoDBObject ("STACK_TRACE" -> stackTrace)
        val inc = $inc("count" -> 1) ++ $set("lastDate" -> ISODateTimeFormat.dateTime().print(new DateTime()))
        val existing = Mongo.mongoColl(mode).findAndModify(query, inc);

        if (existing.isDefined) {
          log.info("Existing stack trace found and count incremented");
        } else {
          log.info("New stack trace found")

          // create a new object
          val builder = MongoDBObject.newBuilder
          params foreach (param => {
              builder += param._1 -> param._2
          })

          builder += "count" -> 1
          builder += "status" -> "new"
          builder += "lastDate" -> ISODateTimeFormat.dateTime().print(new DateTime())
          val updateObj = builder.result()

          log.info("Saving new trace")
          Mongo.mongoColl(mode).insert(updateObj);
        }
      }


    } else {
      log.warn("bad request? did not find any valid fields")
    }
  }

}

class CrashLog  {
  var id : Option[ObjectId] = None
  var stackTrace : Option[String] = None
  var androidVersion : Option[String] = None
  var version : Option[String] = None
  var model : Option[String] = None
  var display : Option[String] = None
  var date : Option[DateTime] = None
  var count : Option[Int] = None
  var lastDate : Option[DateTime] = None
  var mode : Option[String] = None
  var developerMode : Option[String] = None
}