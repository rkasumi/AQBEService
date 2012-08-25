package parse

import java.text.SimpleDateFormat
import java.util.Date

import scala.collection.JavaConversions.asScalaBuffer
import scala.collection.JavaConversions.asScalaSet
import scala.util.control.Exception.allCatch
import scala.xml.NodeSeq

import org.openehr.am.archetype.constraintmodel.ArchetypeInternalRef
import org.openehr.am.archetype.constraintmodel.CComplexObject
import org.openehr.am.archetype.constraintmodel.CObject
import org.openehr.am.archetype.constraintmodel.CPrimitiveObject
import org.openehr.am.archetype.constraintmodel.ConstraintRef
import org.openehr.am.archetype.constraintmodel.primitive.CInteger
import org.openehr.am.archetype.constraintmodel.primitive.CReal
import org.openehr.am.openehrprofile.datatypes.quantity.CDvOrdinal
import org.openehr.am.openehrprofile.datatypes.quantity.CDvQuantity
import org.openehr.am.openehrprofile.datatypes.text.CCodePhrase
import org.openehr.rm.support.basic.Interval

/**
 * 言語ごとのOntology
 * @param List(ID,Title,Description)
 */
object Ontology {
  def apply(ontology: List[(String, String, String)]) = new Ontology(ontology)
}
class Ontology(ontology: List[(String, String, String)]) {
  private val text = Map() ++ (ontology map (o ⇒ (o._1, o._2)))
  private val description = Map() ++ (ontology map (o ⇒ (o._1, o._3)))
  def getText(key: String) = text.get(key)
  def getDescription(key: String) = description.get(key)
  override def toString = (ontology map { o ⇒ "[" + o._1 + "] = " + o._2 + "\n\t" + o._3 + "\n" }) mkString "\n"
}

/**
 * ADL *Listの要素型のスーパークラス
 */
sealed abstract class AdlAttribute {
  val path: String
  val name: String
  override def toString = name + " : " + path + "\n"
}
object AdlAttribute {
  def apply(cObject: CObject, ontology: Ontology): AdlAttribute = {
    val name = ontology.getText(cObject.getNodeID).getOrElse("")

    cObject.getRmTypeName match {
      case "ELEMENT" ⇒ DvElement(name, cObject, ontology)
      case "CLUSTER" ⇒ DvCluster(name, cObject.path, cObject, ontology)
    }
  }
}

/**
 * ADL attributeのエレメント要素
 */
abstract class DvElement extends AdlAttribute {
  val name: String
  val path: String
}
object DvElement {
  def apply(name: String, cObject: CObject, ontology: Ontology) = {
    try {
      val clist = cObject.asInstanceOf[CComplexObject].getAttributes.get(0).getChildren
      clist.size match {
        case 1 ⇒ generateElement(name, clist.get(0), ontology)
        case _ ⇒ DvMultipuleElements(name, cObject.path, clist.toList, ontology)
      }
    } catch {
      case _ ⇒ cObject.isInstanceOf[ArchetypeInternalRef] match { // TODO リファレンス型
        case true  ⇒ ArchetypeReference("**References", cObject.asInstanceOf[ArchetypeInternalRef].getTargetPath)
        case false ⇒ DvAny(name, cObject.path)
      }
    }
  }

  def generateElement(name: String, cObject: CObject, ontology: Ontology) = {
    val path = cObject.path
    cObject.getRmTypeName match {
      case "DvQuantity"    ⇒ DvQuantity(name, path, cObject)
      case "DV_CODED_TEXT" ⇒ DvCodedText(name, path, cObject, ontology)
      case "DV_BOOLEAN"    ⇒ DvBoolean(name, path)
      case "DV_TEXT"       ⇒ DvText(name, path)
      case "DV_COUNT"      ⇒ DvCount(name, path, cObject)
      case "DvOrdinal"     ⇒ DvOrdinal(name, path, cObject, ontology)
      case "DV_MULTIMEDIA" ⇒ DvMultiMedia(name, path, cObject)
      case "DV_DATE_TIME"  ⇒ DvDateTime(name, path)
      case "DV_INTERVAL"   ⇒ DvInterval(name, path, cObject)
      case "DV_PROPORTION" ⇒ DvProportion(name, path, cObject)
      case "DV_URI"        ⇒ DvURI(name, path)
      case _               ⇒ DvAny(name, path)
    }
  }
}

/**
 * リファレンス型
 */
case class ArchetypeReference(name: String, path: String) extends DvElement {
  override def toString = "ArchetypeRef: " + path + "\n"
}

/**
 * 真偽値を保存するクラス
 */
case class DvBoolean(name: String, path: String) extends DvElement {
  override def toString = super.toString + " <TRUE/FALSE>\n"
}

/**
 * テキストフィールドを保存するクラス
 */
case class DvText(name: String, path: String) extends DvElement {
  override def toString = super.toString + " <FreeText>\n"
}

/**
 * 日付型を保存するクラス
 */
case class DvDateTime(name: String, path: String) extends DvElement {
  override def toString = super.toString + " <1990/01/01>\n"
}

/**
 * URLを保存するクラス
 */
case class DvURI(name: String, path: String) extends DvElement {
  override def toString = super.toString + " <http://>\n"
}

/**
 * 名前とパスのみを保存するクラス
 */
case class DvAny(name: String, path: String) extends DvElement {
  override def toString = super.toString + " [Any]\n"
}

/**
 * 選択式リストを保存するクラス
 */
case class DvCodedText(name: String, path: String, codeList: List[String]) extends DvElement {
  override def toString = super.toString + codeList.mkString("\n") + "\n"
}
object DvCodedText {
  def apply(name: String, path: String, cObject: CObject, ontology: Ontology) = {
    val codeList = allCatch opt {
      cObject.asInstanceOf[CComplexObject].getAttributes.get(0).getChildren.get(0) match {
        case c: CCodePhrase   ⇒ c.getCodeList.map(ontology.getText(_).getOrElse("")).toList
        case c: ConstraintRef ⇒ List(ontology.getText(c.getReference).getOrElse(""))
      }
    }
    new DvCodedText(name, path, codeList.getOrElse(List()))
  }
}

/**
 * ハッシュリストを保存するクラス
 */
case class DvOrdinal(name: String, path: String, codeList: Map[Int, Option[String]]) extends DvElement {
  override def toString = super.toString + (codeList.map { o ⇒ o._1 + " - " + o._2.getOrElse("") } mkString "\n") + "\n"
}
object DvOrdinal {
  def apply(name: String, path: String, cObject: CObject, ontology: Ontology) = {
    val codeList = Map() ++ cObject.asInstanceOf[CDvOrdinal].getList.map {
      o ⇒ o.getValue -> ontology.getText(o.getSymbol.getCodeString)
    }
    new DvOrdinal(name, path, codeList)
  }
}

/**
 * マルチメディア型を保存するクラス
 */
case class DvMultiMedia(name: String, path: String, codeList: List[String]) extends DvElement {
  override def toString = super.toString + (codeList mkString "\n") + "\n"
}
object DvMultiMedia {
  def apply(name: String, path: String, cObject: CObject) = {
    val codeList = allCatch opt {
      cObject.asInstanceOf[CComplexObject].getAttributes.get(0).getChildren.get(0).asInstanceOf[CCodePhrase].getCodeList map {
        c ⇒ Settings.Multimedia.getOrElse(c.toInt, "")
      } toList
    }
    new DvMultiMedia(name, path, codeList.getOrElse(List()))
  }
}

/**
 * 数値の範囲が存在するかを確認するトレイト
 */
trait CheckInterval {
  protected def isSomeDouble(t: Option[Interval[java.lang.Double]]) = t match {
    case Some(i) ⇒ (isNull(i.getLower), isNull(i.getUpper))
    case None    ⇒ (None, None)
  }
  protected def isSomeInteger(t: Option[Interval[java.lang.Integer]]) = t match {
    case Some(i) ⇒ (isNull(i.getLower), isNull(i.getUpper))
    case None    ⇒ (None, None)
  }

  protected def isNull(t: java.lang.Integer): Option[Int] = t match {
    case null ⇒ None
    case i    ⇒ Option(i.toInt)
  }
  protected def isNull(t: java.lang.Double): Option[Double] = t match {
    case null ⇒ None
    case i    ⇒ Option(i.toDouble)
  }
}

/**
 * 比率を保存するクラス
 */
case class DvProportion(name: String, path: String,
                        minNum: Option[Double], maxNum: Option[Double],
                        minDen: Option[Double], maxDen: Option[Double]) extends DvElement {
  override def toString =
    super.toString +
      "Numerator: min->" + minNum.getOrElse(0.0) + " max->" + maxNum.getOrElse(0.0) + "\n" +
      "Denominator: min->" + minDen.getOrElse(0.0) + " max->" + maxDen.getOrElse(0.0) + "\n"
}
object DvProportion extends CheckInterval {
  def apply(name: String, path: String, cObject: CObject) = {
    val numerator = isSomeDouble(
      allCatch opt {
        cObject.asInstanceOf[CComplexObject].
          getAttributes.get(0).getChildren.get(0).asInstanceOf[CPrimitiveObject].
          getItem.asInstanceOf[CReal].getInterval
      }
    )
    val denominator = isSomeDouble(
      allCatch opt {
        cObject.asInstanceOf[CComplexObject].
          getAttributes.get(1).getChildren.get(0).asInstanceOf[CPrimitiveObject].
          getItem.asInstanceOf[CReal].getInterval
      }
    )

    new DvProportion(name, path, numerator._1, numerator._2, denominator._1, denominator._2)
  }
}

/**
 * 整数を保存するクラス
 * リストがすべてNoneのものもあり
 */
case class DvCount(name: String, path: String, min: Option[Int], max: Option[Int]) extends DvElement {
  override def toString = super.toString + "max->" + min.getOrElse(0) + " max->" + max.getOrElse(0) + "\n"
}
object DvCount extends CheckInterval {
  def apply(name: String, path: String, cObject: CObject) = {
    val range = isSomeInteger(
      allCatch opt {
        cObject.asInstanceOf[CComplexObject].
          getAttributes.get(0).getChildren.get(0).asInstanceOf[CPrimitiveObject].
          getItem.asInstanceOf[CInteger].getInterval
      }
    )
    new DvCount(name, path, range._1, range._2)
  }
}

/**
 * 量を保存するクラス
 * リストがすべてNoneのものもあり
 */
case class DvQuantity(name: String, path: String,
                      min: List[Option[Double]], max: List[Option[Double]], unit: List[String]) extends DvElement {
  override def toString = {
    val range = min.zipAll(max, None, None).zipAll(unit, (None, None), "")
    super.toString + (range.map {
      item ⇒ "min->" + item._1._1.getOrElse(0.0) + " max->" + item._1._2.getOrElse(0.0) + " units->" + item._2
    } mkString "\n") + "\n"
  }
}
object DvQuantity extends CheckInterval {
  def apply(name: String, path: String, cObject: CObject) = {
    val magnitude = allCatch opt {
      cObject.asInstanceOf[CDvQuantity].getList.toList map { item ⇒ isSomeDouble(Some(item.getMagnitude)) }
    }
    val units = allCatch opt {
      cObject.asInstanceOf[CDvQuantity].getList.toList map { item ⇒ item.getUnits }
    }
    new DvQuantity(name, path, magnitude.getOrElse(List()).map(_._1), magnitude.getOrElse(List()).map(_._2), units.getOrElse(List()))
  }
}

/**
 * 範囲を入力するリスト
 * 複数のCount,Quantityを保存する
 */
case class DvInterval(name: String, path: String, interval: List[DvElement]) extends DvElement {
  override def toString = super.toString + (interval map { _.toString } mkString "\n") + "\n"
}
object DvInterval {
  def apply(name: String, path: String, cObject: CObject) = {
    val dvQuantityPattern = """.*DV_QUANTITY.*""".r
    val dvCountPattern = """.*DV_COUNT.*""".r
    val interval = cObject.getRmTypeName match {
      case dvQuantityPattern() ⇒ cObject.asInstanceOf[CComplexObject].getAttributes.toList map { item ⇒ DvQuantity(name, path, item.getChildren.get(0)) }
      case dvCountPattern()    ⇒ cObject.asInstanceOf[CComplexObject].getAttributes.toList map { item ⇒ DvCount(name, path, item.getChildren.get(0)) }
      case _                   ⇒ List()
    }
    new DvInterval(name, path, interval)
  }
}

/**
 * 入れ子構造 CLUSTER
 * 入れ子が存在すればCLUSTER
 * 存在しなければArchetypeSlot
 * をそれぞれ生成
 */
case class DvCluster(name: String, path: String, cluster: List[AdlAttribute]) extends AdlAttribute {
  override def toString = super.toString + (cluster map { _.toString } mkString (" ===> ", " ===> ", "")) + "\n"
}
object DvCluster {
  def apply(name: String, path: String, cObject: CObject, ontology: Ontology) = {
    val cluster = allCatch opt {
      cObject.asInstanceOf[CComplexObject].getAttributes.get(0).getChildren.toList map {
        AdlAttribute(_, ontology)
      }
    }
    cluster match {
      case Some(c) ⇒ new DvCluster(name, path, c)
      case None    ⇒ new DvAny(name, path)
    }
  }
}

/**
 * マルチプル型を保存する
 * 複数のelementsとタイプを保存する
 */
case class DvMultipuleElements(name: String, path: String, elements: List[(String, AdlAttribute)]) extends DvElement {
  override def toString = super.toString +
    (elements map { item ⇒ item._1.toString + "\n" + item._2.toString } mkString (" |-- ", " |-- ", "")) + "\n"
}
object DvMultipuleElements {
  def apply(name: String, path: String, clist: List[CObject], ontology: Ontology) = {
    val elements = clist map {
      cObject ⇒ (cObject.getRmTypeName.replaceAll("DV_", "").replaceAll("Dv", ""), DvElement.generateElement(name, cObject, ontology))
    }
    new DvMultipuleElements(name, path, elements)
  }
}
