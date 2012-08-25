package parse

import scala.xml.XML

/**
 * ADL名を読み込んで一致するXMLの結果を返す
 * ただしADL名は拡張子なし
 */
object ConceptParser {
  def apply(name: String) = {
    val nameList = name.split(",") toList
    val html = XML.load(Settings.XmlPath)
    nameList map {
      name =>
      val xmlList = (html \\ "a") map (Settings.XmlPath + _.text) map (XmlParser(_))
      xmlList collectFirst {case list if list.exists(_ == name) => list}
    }
  }

  def XmlParser(path: String) = {
    val xml = XML.load(path)
    val isAdl = """(openEHR-EHR.*)""".r
    xml \\ "@archetype_node_id" map(_.text) collect {case isAdl(node) => node} distinct
  }
}
