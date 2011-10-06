package model

import _root_.controllers.Mongo
import collection.mutable.HashSet
import com.mongodb.casbah.Imports._
import collection.JavaConversions._
import com.google.gson.Gson
import util.Logging
import org.joda.time.DateTime
import org.joda.time.format.ISODateTimeFormat

object CrashReport extends Logging {

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
  def findCrashReports (mode: String) : List[CrashReport] = {
    // fetch the data (transform this to a List[CrashLog])
    val objs : Iterator[CrashReport] = for (obj <- Mongo.crashreports(mode).find().sort(MongoDBObject("lastDate" -> -1))) yield {

      // get a few mandatory fields
      val id = obj.getAs[ObjectId]("_id").get
      val stackTrace = obj.getAs[String]("stackTrace").get

      var cr = new CrashReport(id, stackTrace)
      cr.originalDate = obj.getAs[DateTime]("originalDate")
      cr.lastDate = obj.getAs[DateTime]("lastDate")
      cr.status = obj.getAs[String]("status").getOrElse("NEW")
      var instances = obj.getAs[BasicDBList]("instances").get.toList
      var aslist = instances.collect { case s: ObjectId => s}
      cr.instances = aslist
      cr
    }

    return objs.toList
  }

  def findCrashLog(mode: String, id: String) : Option[CrashLog] = {

    val query = MongoDBObject("_id" -> new ObjectId(id))
    val objOption = Mongo.crashlogs(mode).findOne(query)
    if (objOption.isDefined) {

      val obj = objOption.get

      val cl = new CrashLog()

      cl.androidVersion = obj.getAs[String]("ANDROID_VERSION")
      cl.version = obj.getAs[String]("APP_VERSION_NAME")
      cl.stackTrace = obj.getAs[String]("STACK_TRACE")
      cl.model = obj.getAs[String]("PHONE_MODEL")
      cl.display = obj.getAs[String]("DISPLAY")
      cl.date = obj.getAs[DateTime]("USER_CRASH_DATE")
      cl.id = obj.getAs[ObjectId]("_id")
      cl.developerMode = obj.getAs[String]("DEVELOPER_MODE")
      cl.mode = obj.getAs[String]("MODE")

      return Option(cl)
    }

    return None
  }

  def findCrashReport (mode: String, id : String ) : Option[CrashReport] = {
    val query = MongoDBObject("_id" -> new ObjectId(id))
    val objOption = Mongo.crashreports(mode).findOne(query)
    if (objOption.isDefined) {

      val obj = objOption.get

      // get a few mandatory fields
      val id = obj.getAs[ObjectId]("_id").get
      val stackTrace = obj.getAs[String]("stackTrace").get

      val cr = new CrashReport(id, stackTrace)

      cr.originalDate = obj.getAs[DateTime]("originalDate")
      cr.lastDate = obj.getAs[DateTime]("lastDate")
      cr.status = obj.getAs[String]("status").getOrElse("NEW")

      var instances = obj.getAs[BasicDBList]("instances").get.toList
      var aslist = instances.collect { case s: ObjectId => s}
      cr.instances = aslist

      return Option(cr)
    }

    return None
  }

  /**
   * Current # of logs in the system
   */
  def count(mode : String) : Long = {
    return Mongo.crashreports(mode).count
  }

  /**
   * Saves an object to the db.  If an existing CrashReport with the same stack trace exists, that one
   * will be updated, otherwise, a new one will be created with this log.
   */
  def save (mode: String, params : Map[String,String]) : Unit = {

    // first look for an existing object w/ this stack trace, see if we can update it

    if (!params.isEmpty) {

      val stackTrace = params.get("STACK_TRACE")

      if (stackTrace.isDefined) {

        // create a new object
        val builder = MongoDBObject.newBuilder
        params foreach (param => {
            builder += param._1 -> param._2
        })

        log.info("Saving new trace")
        val stobj = builder.result()
        Mongo.crashlogs(mode).insert(stobj)

        // need the object id from crashlogs
        val id = stobj.getAs[ObjectId]("_id").get

        val query = MongoDBObject ("stackTrace" -> stackTrace)

        val now = ISODateTimeFormat.dateTime().print(new DateTime())
        val inc = $set("lastDate" -> now) ++ $set("status" -> "new") ++ $push("instances" -> id)

        val existing = Mongo.crashreports(mode).findAndModify(query, inc)

        if (existing.isDefined) {
          log.info("Existing crash report found and updated");
        } else {
          log.info("New stack trace found")

          // create a new object
          val builder = MongoDBObject.newBuilder
          builder += "status" -> "NEW"
          builder += "instances" -> List(id)
          builder += "originalDate" -> now
          builder += "lastDate" -> now
          builder += "stackTrace" -> stackTrace

          log.info("Saving new crash report")
          Mongo.crashreports(mode).insert(builder.result());
        }
      }


    } else {
      log.warn("bad request? did not find any valid fields")
    }
  }

}

/**
 * Represents an individual crash report from the field.
 */
class CrashLog  {
  var id : Option[ObjectId] = None
  var androidVersion : Option[String] = None
  var version : Option[String] = None
  var model : Option[String] = None
  var display : Option[String] = None
  var date : Option[DateTime] = None
  var mode : Option[String] = None
  var developerMode : Option[String] = None
  var stackTrace : Option[String] = None
}

/**
 * Represents statics for a particular crash.
 */
case class CrashReport(id : ObjectId, stackTrace : String) {

  /**
   * Ordered list of individual reports
   */
  var instances : List[ObjectId] = List()

  /**
   * The status of the report ("NEW" or "OLD")
   */
  var status : String = "NEW"

  /**
   * Original date it was reported
   */
  var originalDate : Option[DateTime] = None

  /**
   * Last date it was reported
   */
  var lastDate : Option[DateTime] = None

  def numReports() : Int = {
    return instances.size()
  }

  def summary() : String = {
    val StackParser = """(\w+):.*""".r
    stackTrace match {
      case StackParser(aaa) => return aaa;
      case _ => return "UNKNOWN";
    }
  }

}

