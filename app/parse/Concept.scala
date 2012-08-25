package parse

import scala.collection.JavaConversions.asScalaBuffer
import scala.collection.JavaConversions.seqAsJavaList
import scala.util.control.Exception.allCatch

import org.openehr.am.archetype.Archetype
import org.openehr.am.archetype.constraintmodel.CAttribute
import org.openehr.am.archetype.constraintmodel.CComplexObject
import org.openehr.am.archetype.constraintmodel.CMultipleAttribute

/**
 * ADLデフォルトクラス
 */
abstract class Concept { val name: String }
object Concept {
  def apply(id: String, name: String, adl: Archetype, ontology: Ontology): Option[Concept] = {
    val definitionList = adl.getDefinition.getAttributes.toList
    val composition = """openEHR-EHR-COMPOSITION\.(.*)\.v.+""".r
    val section = """openEHR-EHR-SECTION\.(.*)\.v.+""".r
    val observation = """openEHR-EHR-OBSERVATION\.(.*)\.v.+""".r
    val itemtree = """openEHR-EHR-ITEM_TREE\.(.*)\.v.+""".r
    val evaluation = """openEHR-EHR-EVALUATION\.(.*)\.v.+""".r
    val instruction = """openEHR-EHR-INSTRUCTION\.(.*)\.v.+""".r
    val action = """openEHR-EHR-ACTION\.(.*)\.v.+""".r
    val adminentry = """openEHR-EHR-ADMIN_ENTRY\.(.*)\.v.+""".r
    name match {
      case composition(adlName) ⇒ Some(AdlComposition(adlName, definitionList, ontology))
      case section(adlName)     ⇒ Some(AdlSection(adlName, definitionList, ontology))
      case observation(adlName) ⇒ Some(AdlObservation(adlName, definitionList, ontology))
      case itemtree(adlName)    ⇒ Some(AdlItemTree(adlName, definitionList, ontology))
      case evaluation(adlName)  ⇒ Some(AdlEvaluation(adlName, definitionList, ontology))
      case instruction(adlName) ⇒ Some(AdlInstruction(adlName, definitionList, ontology))
      case action(adlName)      ⇒ Some(AdlAction(adlName, definitionList, ontology))
      case adminentry(adlName)  ⇒ Some(AdlAdminEntry(adlName, definitionList, ontology))
      case _                    ⇒ None
    }
  }
}

/**
 * ADL COMPOSITION
 */
case class AdlComposition(name: String, contextList: List[AdlAttribute]) extends Concept
object AdlComposition {
  def apply(name: String, definitionList: List[CAttribute], ontology: Ontology) = {
    val contextList = {
      allCatch opt {
        definitionList.find(_.path == "/context").get.
          getChildren.get(0).asInstanceOf[CComplexObject].
          getAttributes.get(0).getChildren.get(0).asInstanceOf[CComplexObject].
          getAttributes.get(0).getChildren.toList
      } getOrElse (List())
    } map { AdlAttribute(_, ontology) }

    new AdlComposition(name, contextList)
  }
}

/**
 * ADL SECTION
 */
case class AdlSection(name: String) extends Concept
object AdlSection {
  def apply(name: String, definitionList: List[CAttribute], ontology: Ontology) = {
    new AdlSection(name)
  }
}

/**
 * ADL OBSERVATION
 */
case class AdlObservation(name: String, dataList: List[AdlAttribute], stateList: List[AdlAttribute], protocolLost: List[AdlAttribute]) extends Concept
object AdlObservation {
  def apply(name: String, definitionList: List[CAttribute], ontology: Ontology) = {
    val dataList = {
      allCatch opt {
        definitionList.get(0).getChildren.get(0).asInstanceOf[CComplexObject].
          getAttributes.get(0).getChildren.get(0).asInstanceOf[CComplexObject].
          getAttributes.find(_.getRmAttributeName == "data").get.getChildren.get(0).asInstanceOf[CComplexObject].
          getAttributes.get(0).asInstanceOf[CAttribute].
          getChildren.toList
      } getOrElse (List())
    } map { AdlAttribute(_, ontology) }

    val stateList = {
      allCatch opt {
        definitionList.get(0).getChildren.get(0).asInstanceOf[CComplexObject].
          getAttributes.get(0).getChildren.get(0).asInstanceOf[CComplexObject].
          getAttributes.find(_.getRmAttributeName == "state").get.getChildren.get(0).asInstanceOf[CComplexObject].
          getAttributes.get(0).asInstanceOf[CAttribute].
          getChildren.toList
      } getOrElse (List())
    } map { AdlAttribute(_, ontology) }

    val protocolList = {
      allCatch opt {
        definitionList.get(1).getChildren.get(0).asInstanceOf[CComplexObject].
          getAttributes.get(0).asInstanceOf[CMultipleAttribute].
          getChildren.toList
      } getOrElse (List())
    } map { AdlAttribute(_, ontology) }

    new AdlObservation(name, dataList, stateList, protocolList)
  }
}

/**
 * ADL ITEM_TREE
 */
case class AdlItemTree(name: String, dataList: List[AdlAttribute]) extends Concept
object AdlItemTree {
  def apply(name: String, definitionList: List[CAttribute], ontology: Ontology) = {
    val dataList = {
      allCatch opt {
        definitionList.get(0).asInstanceOf[CMultipleAttribute].
          getChildren.toList
      } getOrElse (List())
    } map { AdlAttribute(_, ontology) }

    new AdlItemTree(name, dataList)
  }
}

/**
 * ADL EVALUATION
 */
case class AdlEvaluation(name: String, dataList: List[AdlAttribute]) extends Concept
object AdlEvaluation {
  def apply(name: String, definitionList: List[CAttribute], ontology: Ontology) = {
    val dataList = {
      allCatch opt {
        definitionList.get(0).getChildren.get(0).asInstanceOf[CComplexObject].
          getAttributes.get(0).getChildren.toList
      } getOrElse (List())
    } map { AdlAttribute(_, ontology) }

    new AdlEvaluation(name, dataList)
  }
}

/**
 * ADL INSTRUCTION
 */
case class AdlInstruction(name: String, dataList: List[AdlAttribute]) extends Concept
object AdlInstruction {
  def apply(name: String, definitionList: List[CAttribute], ontology: Ontology) = {
    val dataList = {
      allCatch opt {
        definitionList.get(0).getChildren.get(0).asInstanceOf[CComplexObject].
          getAttributes.get(0).getChildren.get(0).asInstanceOf[CComplexObject].
          getAttributes.get(0).getChildren.toList
      } getOrElse (List())
    } map { AdlAttribute(_, ontology) }

    new AdlInstruction(name, dataList)
  }
}

/**
 * ADL ACTION
 */
case class AdlAction(name: String, dataList: List[AdlAttribute]) extends Concept
object AdlAction {
  def apply(name: String, definitionList: List[CAttribute], ontology: Ontology) = {
    val dataList = {
      allCatch opt {
        definitionList.get(1).getChildren.get(0).asInstanceOf[CComplexObject].
          getAttributes.get(0).getChildren.toList
      } getOrElse (List())
    } map { AdlAttribute(_, ontology) }

    new AdlAction(name, dataList)
  }
}

/**
 * ADL ADMIN_ENTRY
 */
case class AdlAdminEntry(name: String, dataList: List[AdlAttribute]) extends Concept
object AdlAdminEntry {
  def apply(name: String, definitionList: List[CAttribute], ontology: Ontology) = {
    val dataList = {
      allCatch opt {
        definitionList.get(0).getChildren.get(0).asInstanceOf[CComplexObject].
          getAttributes.get(0).getChildren.toList
      } getOrElse (List())
    } map { AdlAttribute(_, ontology) }

    println(dataList.length);

    new AdlAdminEntry(name, dataList)
  }
}
