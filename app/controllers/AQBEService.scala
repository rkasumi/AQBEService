package controllers

import com.mongodb.casbah.DBObject
import com.mongodb.casbah.MongoConnection
import com.mongodb.casbah.ServerAddress
import com.mongodb.util.{JSON => mongoJSON}
import play.api.mvc.Action
import play.api.mvc.Controller
import play.api.mvc.SimpleResult

object AQBEService extends Controller {

  def find = Action(parse.json) { request =>
    val db = MongoConnection(new ServerAddress("wako3.u-aizu.ac.jp",27017))("aqbe")("docs")
    val cond = mongoJSON.parse(request.body.toString).asInstanceOf[DBObject]
    val result = db.find(cond).toList
    Ok("""{"result":[""" + result.mkString(",\n") + "]}").as(JSON)
  }

  def findAll = Action {
    val db = MongoConnection(new ServerAddress("wako3.u-aizu.ac.jp",27017))("aqbe")("docs")
    val result = db.find.toList
    Ok("""{"result":[""" + result.mkString(",\n") + "]}").as(JSON)
  }

  def insert = Action(parse.json) { request =>
    val db = MongoConnection(new ServerAddress("wako3.u-aizu.ac.jp",27017))("aqbe")("docs")
    val data = mongoJSON.parse(request.body.toString).asInstanceOf[DBObject]
    db.insert(data)
    Ok("""{"result":"OK"}""").as(JSON)
  }

}
