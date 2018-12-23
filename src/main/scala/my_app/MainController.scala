package hmtIndex 
import com.thoughtworks.binding.{Binding, dom}
import com.thoughtworks.binding.Binding.{BindingSeq, Var, Vars}
import scala.scalajs.js
import scala.scalajs.js._
import js.annotation._
import scala.concurrent._
//import ExecutionContext.Implicits.global
import collection.mutable
import collection.mutable._
import scala.scalajs.js.Dynamic.{ global => g }
import org.scalajs.dom._
import org.scalajs.dom.ext._
import org.scalajs.dom.raw._
import edu.holycross.shot.cite._
import edu.holycross.shot.scm._
import edu.holycross.shot.ohco2._
import edu.holycross.shot.citeobj._
import edu.holycross.shot.citejson._

import edu.holycross.shot.greek._

import monix.execution.Scheduler.Implicits.global
import monix.eval._

import scala.scalajs.js.annotation.JSExport

@JSExportTopLevel("hmtIndex.MainController")
object MainController {


	/* 
		Initiate app 
	*/
	@JSExport
	def main(defaultServiceUrl:String, defaultAppUrl:String, defaultICTUrl:String, dseMSMap:Array[js.Object]): Unit = {
		dom.render(document.body, MainView.mainDiv)
		MainModel.serviceUrl.value = defaultServiceUrl
		MainModel.appUrl.value = defaultAppUrl
		MainModel.ictUrl.value = defaultICTUrl
		val scalaMSMap:Vector[MainModel.Facsimile] = MainController.getFacsFromJS(dseMSMap)		
		for (us <- scalaMSMap){ 
			MainModel.facsimiles.value += us
		}
		g.console.log(s"using: ${MainModel.serviceUrl.value}")
		// If we have some facsimile data, load the first one	
		MainModel.facsimiles.value size match {
			case n if (n > 0) => {
				val u:Cite2Urn = MainModel.facsimiles.value(0).dseUrn
				MainController.changeMS(u)
			}
			case _ => {
				MainController.updateUserMessage("No valid facsimile data!", 2)
			}
		}

	}

	

	/*
	 	Handles displaying messages to the user, color-coded according to type.
	 	Fades after a period of time, defined in the setTimeout().
	*/
	def updateUserMessage(msg: String, alert: Int): Unit = {
		MainModel.userMessageVisibility.value = "app_visible"
		MainModel.userMessage.value = msg
		alert match {
			case 0 => MainModel.userAlert.value = "default"
			case 1 => MainModel.userAlert.value = "wait"
			case 2 => MainModel.userAlert.value = "warn"
		}
		js.timers.clearTimeout(MainModel.msgTimer)
		MainModel.msgTimer = js.timers.setTimeout(80000){ MainModel.userMessageVisibility.value = "app_hidden" }
	}

	/* 
		We get a funky JS Object from the HTML containers… this turns it into a Vector of
	 	Facsimile objects (or an empty vector)
	*/
	def getFacsFromJS(jsArray:Array[js.Object]):Vector[MainModel.Facsimile] = {
		try {
			val tempVec:Vector[js.Dictionary[String]] = jsArray.asInstanceOf[Array[js.Dictionary[String]]].toVector
			val finalVec:Vector[MainModel.Facsimile] = {
				tempVec.map(jso => {
					val scalaObj = jso.asInstanceOf[js.Dictionary[String]].toMap
					val thisUrn:Cite2Urn = Cite2Urn(scalaObj("urn"))
					val thisText:CtsUrn = CtsUrn(scalaObj("text"))
					val thisLabel:String = scalaObj("label")
					val thisUrl:String = scalaObj("url")
					MainModel.Facsimile(thisUrn, thisText, thisLabel, thisUrl)
				})
			}
			finalVec
		} catch {
			case e:Exception => MainController.updateUserMessage(s"Error in parsing available facsimiles: ${e}",2)
			Vector()
		}
	}

	/* *****************************************
	Change the current Manuscript
	
	Called on init, or from MainView.collectionSelector
	******************************************** */	
	def changeMS(u:Cite2Urn):Unit = {
		val newColl:Vector[MainModel.Facsimile] = {
			MainModel.facsimiles.value.toVector.filter( c => c.dseUrn == u)
		}
		MainModel.folios.value.clear
		newColl.size match {
			case n if (n > 0) => {
				// If there is more than one, something is wrong, but grab the first…
				MainModel.currentFacsimile.value = Some(newColl(0))
				//MainController.queryDseObjects(newColl(0)._1)
			}
			case _ => {
				MainModel.currentFacsimile.value = None	
			}

		}
		g.console.log(s"Changed MS to: ${MainModel.currentFacsimile.value}")
	}

	/*
		Use AJAX request to get remote data
		`callback` is the name of a function that should take a single parameter of type String
	*/
	def getJson(callback: (String, Option[Urn]) => Unit, query:String, url: String = MainModel.serviceUrl.value, urn:Option[Urn] = None):Unit = {

		val xhr = new XMLHttpRequest()
		xhr.open("GET", s"${url}${query}" )
		xhr.onload = { (e: Event) =>
			if (xhr.status == 200) {
				val contents:String = xhr.responseText
				callback(contents, urn)
			} else {
				MainController.updateUserMessage(s"Request for info on remote library failed with code ${xhr.status}",2)
			}
		}
		xhr.send()
	}


	/* *****************************************
	Routines to validate a requested passage

	Gets called from:

		MainView.passageInput.urnValidatingKeyUpHandler
		MainController.queryValidReff


	Passage must be in the form of:
		1.1
		1.1-1.10
	******************************************** */

	def validateSinglePassage(rawPassage:String):Option[String] = {
		var returnMessage:Option[String] = None
		val passage = s" ${rawPassage} "

		//check for more than one period
		passage.filter(c => c == '.').size match {
			case n if (n < 1) => {
				returnMessage = Some(s"""Cite Iliad passage with Book and Line, like "1.10". """)	
			}
			case n if (n > 1) => {
				returnMessage = Some(s"""The citation "${rawPassage} has too many components; cite the Iliad with Book and Line, like "1.10". """)	
			}
			case _ => {
				val trimmed:String = rawPassage.trim
				if (trimmed.split('.').size == 1) {
					returnMessage = Some(s"""Cite Iliad passage with Book and Line, like "1.10". """)	
				}
			}

		}

		returnMessage
	}

	def validatePassage(rawPassage:String):Option[String] = {
		// Let's try to catch errors in passages. Sigh…
		var returnMessage:Option[String] = None
		// pad for splitting
		val passage = s" ${rawPassage} "
		//is Range?
		if (passage.contains("-")) {
			passage.split('-').size match {
				case n if (n < 2) => {
					returnMessage = Some(s"""Invalid range citation. Cite a range of passages like "1.1-1.10" """)
				}
				case n if (n == 2) => {
					val psg1:String = s" ${passage.split('-')(0)} "
					val psg2:String = s" ${passage.split('-')(1)} "
					if (psg1.split('.').size != psg2.split('.').size) {
						returnMessage = Some(s"""Invalid range citation. Both sides of the hyphen must have the same number of components, e.g. "1.1-1.10" """)
					} else {
						val returnMessage1 = validateSinglePassage(psg1)
						if (returnMessage1 == None){
							val returnMessage2 = validateSinglePassage(psg2)
							returnMessage = returnMessage2
						} else {
							returnMessage = returnMessage1
						}

					} 
				}
				case _ => {
					returnMessage = Some(s"""Invalid range citation. Cite a range of passages like "1.1-1.10" """)
				}	
			}
		} else {
			// one passage
			returnMessage = validateSinglePassage(rawPassage)
		}

		returnMessage
	}

	/* *****************************************
	Routines to get ValidReffs for a requested passage

	Gets called from MainModel.changeMS

	Loads an empty FolioMap (MainModel.folioMap); 
	calls MainView.queryButton

	Query the service for its a Collection Catalog
	`http://beta.hpcc.uh.edu/scs/collections/`
	******************************************** */

	def queryValidReff(passage:String):Unit = {
		try {
			MainView.waitingOn
			if (MainModel.currentFacsimile.value == None) {
				throw new Exception("No facsimile selected.")
			} 
			val facs:MainModel.Facsimile = MainModel.currentFacsimile.value.get

			val passageValidation:Option[String] = validatePassage(passage)	
			passageValidation match {
				case Some(e) => throw new Exception(s"Invalid Passage: ${e}")
				case None => {
					val textUrn:CtsUrn = CtsUrn(s"${facs.text}${passage}")
					val queryString:String = s"texts/reff/${textUrn}"

					val task = Task{ MainController.getJson(callback = MainController.processReff, query = queryString, url = MainModel.serviceUrl.value, urn = Some(textUrn)) }
					val future = task.runAsync
				}
			}
		}			
		catch {
			case e:Exception => MainController.updateUserMessage(s"Failed to fetch data: ${e}",2)
		}
	}

	def processReff(jstring:String, urn:Option[Urn] = None):Unit = {
		val cjo:Ohco2Json = Ohco2Json()
		val reffVector:Vector[CtsUrn] = cjo.o2ReffVector(jstring)
		MainView.waitingOff

		// Turn it into a Vector[MainModel.FolioReport]
		val folioReportVector:Vector[MainModel.FolioReport] = {
			reffVector.map( reff => {
				MainModel.FolioReport(
					surfaceUrn = None,
					imageUrn = None,
					textUrn = reff,
					facsimileUrl = None	
				)	
			})
		}

		// Store them
		MainModel.reloadFolios(folioReportVector)

		//One-by-one, find a DSE record
		MainModel.currentFacsimile.value match {
			case Some(f) => {
				MainView.waitingOn
				for (ref <- reffVector) {
					MainController.lookUpFolio(f, ref)	
				}
			}
			case None => {
				MainController.updateUserMessage("No facsimile selected.", 2)
			}
		}
		
	}

	/* *****************************************
	Routines to load a DSE record for a text urn

	Gets called from MainController.processReff 

	Query the service for its a Collection Catalog
	`http://beta.hpcc.uh.edu/scs/objects/find/urnmatch/urn:cite2:hmt:va_dse.v1:?find=urn:cite2:hmt:msA.v1:12r`
	******************************************** */

	def lookUpFolio(facs:MainModel.Facsimile, urn:CtsUrn):Unit = {
		val queryString:String = s"objects/find/urnmatch/${facs.dseUrn}?find=${urn}"
		val task = Task{ MainController.getJson(callback = MainController.addFolio, query = queryString, url = MainModel.serviceUrl.value, urn = Some(urn)) }
		val future = task.runAsync
	}

	def addFolio(jstring:String, urn:Option[Urn] = None):Unit = {
		try {
			val objJson:CiteObjJson = CiteObjJson()
			val vco:Vector[CiteObject] = objJson.vectorOfCiteObjects(jstring)
			vco.size match {
				case n if (n < 1) => throw new Exception(s"No data found for ${urn}.")
				case n if (n == 1) => {
					val newFolios:Vector[MainModel.FolioReport] = MainModel.foliosFromObjects(vco)
					MainModel.updateFolios(newFolios)
				}
				case _ => {
					val newFolios:Vector[MainModel.FolioReport] = MainModel.foliosFromObjects(vco)
					MainModel.updateFolios(newFolios)
				}
			}

		} catch {
			case e:Exception => MainController.updateUserMessage(s"Error getting DSE records for ${urn}: ${e}",2)
		}

	}




}
