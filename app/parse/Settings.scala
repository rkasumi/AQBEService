package parse

/**
 * AQBE Settings
 */
object Settings {
  val AdlPath = "http://wako3.u-aizu.ac.jp:8080/adl/"
  val XmlPath = "http://wako3.u-aizu.ac.jp:8080/adl_sample_xml/"

  val Multimedia = Map(
    425 -> "image/cgm",
    426 -> "image/gif",
    427 -> "image/png",
    428 -> "image/tiff",
    429 -> "image/jpeg",
    417 -> "text/html",
    418 -> "text/plain",
    419 -> "text/rtf",
    420 -> "text/sgml",
    421 -> "text/tab-separated-values",
    422 -> "url-list",
    423 -> "text/xml",
    424 -> "text/xml-external-parsed-entity"
  )

}
