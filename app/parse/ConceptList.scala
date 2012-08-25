package parse

import scala.Option.option2Iterable
import scala.xml.XML

object ConceptList {
  def apply(path: String) = {
    val html = XML.load(path)
    val regrex = """openEHR-EHR-(.*)\.(.*)\..*\..*""".r

    val conceptList = {
      (html \\ "a") map (_.text) map {
        item ⇒
          item match {
            case regrex(dataType, dataName) ⇒ Some((dataType + " : " + dataName, item))
            case _                          ⇒ None
          }
      } flatten
    } sortWith (_._1 < _._1)
    new ConceptList(conceptList.toList)
  }
}
case class ConceptList(concept: List[(String, String)])