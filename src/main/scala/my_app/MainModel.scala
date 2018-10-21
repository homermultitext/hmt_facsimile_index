package hmtIndex 
import com.thoughtworks.binding.{Binding, dom}
import com.thoughtworks.binding.Binding.{BindingSeq, Var, Vars}
import com.thoughtworks.binding.Binding.{Var, Vars}
import com.thoughtworks.binding.dom
import org.scalajs.dom.document
import org.scalajs.dom.raw.Event
import org.scalajs.dom.ext.Ajax
import scala.concurrent
              .ExecutionContext
              .Implicits
              .global

import scala.scalajs.js
import scala.scalajs.js._
import edu.holycross.shot.cite._
import js.annotation._
import edu.holycross.shot.scm._
import edu.holycross.shot.citejson._
import edu.holycross.shot.citeobj._
import scala.scalajs.js.Dynamic.{ global => g }



@JSExportTopLevel("hmtIndex.MainModel")
object MainModel {

	case class Facsimile(dseUrn:Cite2Urn, text:CtsUrn, label:String, url:String) {
		override def toString = this.label
	}

	case class FolioReport(surfaceUrn:Option[Cite2Urn], imageUrn:Option[Cite2Urn], textUrn:CtsUrn, facsimileUrl:Option[String]) {
	}

	val serviceUrl= Var("")
	val appUrl = Var("")
	val ictUrl = Var("")
	val facsimiles = Vars.empty[MainModel.Facsimile] // URN, label, Facsimile URL
	val currentFacsimile = Var[Option[MainModel.Facsimile]](None)
	val currentUserPassage = Var("")
	val validUserPassage = Var(true)

	val userMessage = Var("Main loaded.")
	val userAlert = Var("default")
	val userMessageVisibility = Var("app_hidden")

	var msgTimer:scala.scalajs.js.timers.SetTimeoutHandle = null

	val folios = Vars.empty[MainModel.FolioReport]

	def reloadFolios(folioVector:Vector[MainModel.FolioReport]):Unit = {
		MainModel.folios.value.clear
		for (f <- folioVector){
			MainModel.folios.value += f 
		}
	}

	/* Given a Vector of DSE Object, return a Vector of Folio Reports */
	def foliosFromObjects(objs:Vector[CiteObject]):Vector[MainModel.FolioReport] = {
		try {
			val imagePropName:String = "imageroi"
			val surfacePropName:String = "surface"
			val textPropName:String = "passage"
			val folioVector:Vector[MainModel.FolioReport] = objs.map( o => {
				val u:Cite2Urn = o.urn
				val imagePropUrn:Cite2Urn = propertyUrnFromPropertyName(u, imagePropName)
				val surfacePropUrn:Cite2Urn = propertyUrnFromPropertyName(u, surfacePropName)
				val textPropUrn:Cite2Urn = propertyUrnFromPropertyName(u, textPropName)
				val imageUrn:Cite2Urn = o.propertyValue(imagePropUrn).asInstanceOf[Cite2Urn]
				val surfaceUrn:Cite2Urn = o.propertyValue(surfacePropUrn).asInstanceOf[Cite2Urn]
				val textUrn:CtsUrn = o.propertyValue(textPropUrn).asInstanceOf[CtsUrn]
				val facsUrl:String = s"${currentFacsimile.value.get.url}${surfaceUrn.objectComponent}"
				MainModel.FolioReport(Some(surfaceUrn), Some(imageUrn), textUrn, Some(facsUrl))
			})
			folioVector
		} catch {
			case e:Exception => {
				MainController.updateUserMessage(s"Error: ${e}",2)
				Vector()
			}
		}
	}

	/* The assumption here (untested!) is that all elements in newFolios have the same CtsUrn */
	def updateFolios(newFolios:Vector[MainModel.FolioReport]):Unit = {
		try {
			val oldTemp:Vector[MainModel.FolioReport] = {
				for (f <- MainModel.folios.value) yield f
			} toVector
			val testUrn:CtsUrn = newFolios(0).textUrn
			val testIndex:Int = oldTemp.indexWhere(_.textUrn == testUrn)	
			val firstVec:Vector[MainModel.FolioReport] = oldTemp.slice(0,testIndex)
			val lastVec:Vector[MainModel.FolioReport] = oldTemp.slice( (testIndex+1), oldTemp.length )
			val newTemp:Vector[MainModel.FolioReport] = firstVec ++ newFolios ++ lastVec
			MainModel.folios.value.clear
			for (f <- newTemp) { MainModel.folios.value += f }

			// Do a last little check to see if we've resolved all of them…
			val unresolvedItems:Vector[MainModel.FolioReport] = {
				newTemp.filter(_.surfaceUrn == None)
			}
			if (unresolvedItems.size == 0) { MainView.waitingOff }
		} catch {
			case e:Exception => MainController.updateUserMessage(s"Error: ${e}", 2)
			//MainModel.folios.clear
		}
	}

	/* Should be in CiteLibrary: Given a URN and a property name, generate a property-level URN */
	def propertyUrnFromPropertyName(urn:Cite2Urn, propName:String):Cite2Urn = {
		val returnUrn:Cite2Urn = {
			urn.propertyOption match {
				case Some(po) => urn // just return it!
				case None => {
					val collUrn:Cite2Urn = urn.dropSelector
					val collUrnString:String = collUrn.toString.dropRight(1) // remove colon
					urn.objectComponentOption match {
						case Some(oc) => {
							Cite2Urn(s"${collUrnString}.${propName}:${oc}")
						}
						case None => {
							Cite2Urn(s"${collUrnString}.${propName}:")
						}
					}
				}
			}
		}
		returnUrn
	}	


}
