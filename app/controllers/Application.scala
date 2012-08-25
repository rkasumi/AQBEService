package controllers

import com.github.tototoshi.play2.json.LiftJson
import net.liftweb.json.DefaultFormats
import net.liftweb.json.Extraction
import scala.io.Source
import scala.xml.parsing.{ConstructingParser,XhtmlParser}
import parse._
import play.api.mvc.Action
import play.api.mvc.Controller

object Application extends Controller with LiftJson{

  implicit val formats = DefaultFormats
  
  def index = TODO

  def adl(name: String, lang: String) = Action {
    AdlParser(name).map(_.getConcept(lang)) match {
      case Some(c) => Ok(Extraction.decompose(c))
      case None => NotFound("404 File NotFound")
	}
  }

  def adlList = Action {
    val url = Settings.AdlPath
    val xhtml = XhtmlParser(Source.fromURL(url))
    val allAdl = (xhtml \\ "a").map(_.text.replace(".adl",""))
      Ok(Extraction.decompose(allAdl))
  }

  def concept(name: String, lang: String) = Action {
    ConceptParser(name) match {
      case List(Some(c)) => {
        val conceptList = c.map(x => x -> AdlParser(x).map(_.getConcept(lang))).toMap
        Ok(Extraction.decompose(conceptList))
      }
      case _ => NotFound("404 File NotFound")
    }
  }

}
