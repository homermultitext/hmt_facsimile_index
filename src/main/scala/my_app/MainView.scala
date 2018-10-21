package hmtIndex 

import com.thoughtworks.binding.{Binding, dom}
import com.thoughtworks.binding.Binding.{BindingSeq, Var, Vars}
import scala.scalajs.js
import scala.scalajs.js._
import org.scalajs.dom._
import org.scalajs.dom.ext._
import scala.scalajs.js.Dynamic.{ global => g }
import org.scalajs.dom.raw._
import org.scalajs.dom.document
import org.scalajs.dom.raw.Event
import org.scalajs.dom.ext.Ajax
import edu.holycross.shot.cite._
import edu.holycross.shot.ohco2._
import edu.holycross.shot.greek._
import edu.holycross.shot.citeobj._
import scala.scalajs.js.annotation.JSExport
import js.annotation._
import scala.concurrent._
import monix.execution.Scheduler.Implicits.global
import monix.eval._


@JSExportTopLevel("hmtIndex.MainView")
object MainView {


	@dom
	def mainMessageDiv = {
			<div id="main_message" class={ s"app_message ${MainModel.userMessageVisibility.bind} ${MainModel.userAlert.bind}" } >
				<p> { MainModel.userMessage.bind }  </p>
			</div>
	}



	@dom
	def mainDiv = {
		<div id="main-wrapper">
		<header>
			Homer Multitext Index	
			<span id="app_header_versionInfo">version { BuildInfo.version }</span>
		</header>

		<article id="main_Container">
		{ MainView.mainMessageDiv.bind }
		{ MainView.collectionSelector.bind }
		{ MainView.passageInput.bind }
		{ MainView.folioLinks.bind }
		</article>
		 <div class="push"></div>
		<footer>
		{ footer.bind }
		</footer>
	</div>
	}

	@dom
	def collectionSelector = {
		<label for="manuscriptSelector">Select a Manuscript</label>
		<select id="manuscriptSelector"
		onchange={ event: Event => {
			val thisSelect = document.getElementById("manuscriptSelector").asInstanceOf[HTMLSelectElement]
			thisSelect.value match {
				case "" => // do nothing
				case _ => {
					val u:Cite2Urn = Cite2Urn(thisSelect.value)
					MainController.changeMS(u)				
				}
			}
		} }>

		{ MainView.collectionSelectorOptions.bind }

		</select>
	}

	@dom
	def collectionSelectorOptions = {
		for (ms <- MainModel.facsimiles) yield {
			<option value={ ms.dseUrn.toString }>{ ms.label }</option>
		}
	}

	@dom
	def passageInput = {

		val urnValidatingKeyUpHandler = { event: KeyboardEvent =>
			(event.currentTarget, event.keyCode) match {
				case (input: html.Input, KeyCode.Enter) => {
					event.preventDefault()
				}
				case(input: html.Input, _) =>  {
					val validPassage:Option[String] = MainController.validatePassage(s"${input.value.toString}")
					validPassage match {
						case Some(e) => {
							MainModel.validUserPassage.value = false
							val errorMsgSpan:scala.scalajs.js.Dynamic = js.Dynamic.global.document.getElementById("passageError")
							errorMsgSpan.innerHTML = e
						}
						case None => {
							MainModel.validUserPassage.value = true
							val errorMsgSpan:scala.scalajs.js.Dynamic = js.Dynamic.global.document.getElementById("passageError")
							errorMsgSpan.innerHTML = ""
						}
					}
				}
				case _ =>
			}
		}

		<div id="passageInputDiv">
		<label for="passageInput"
			class={
				MainModel.validUserPassage.bind match {
					case true => "validPassage"
					case false => "invalidPassage"
				}					
			}	
		> <i>Iliad</i> Passage: </label>
		<input id="passageInput" type="text" size={10} placeholder="1.1-1.10"
			class={
				MainModel.validUserPassage.bind match {
					case true => "validPassage"
					case false => "invalidPassage"
				}					
			}	
			onkeyup={ urnValidatingKeyUpHandler }
		/>
		{ queryButton.bind }
		<span id="passageError"
			class={
				MainModel.validUserPassage.bind match {
					case true => "validPassage"
					case false => "invalidPassage"
				}					
			}	></span>
		</div>
	}



	@dom
	def queryButton = {
	<button
		id="querySubmit"
		disabled={ MainModel.validUserPassage.bind == false }
			onclick={ event: Event => {
					MainController.updateUserMessage("Finding folios for passage. Please be patient…",1)
					val thisPassage:String = js.Dynamic.global.document.getElementById("passageInput").value.toString
						
					val task = Task{MainController.queryValidReff(thisPassage)}	
					val future = task.runAsync
				}
			}
		>{ if (MainModel.validUserPassage.bind){
			"Find Folios"
		} else {
			"Enter Valid Passage"
		}
			}
	</button>
	}

	@dom
	def waitingOn = {
		val docBody = js.Dynamic.global.document.getElementById("body")
		docBody.setAttribute("class","loading")
	}

	@dom
	def waitingOff = {
		val docBody = js.Dynamic.global.document.getElementById("body")
		docBody.setAttribute("class","")
	}

	@dom 
	def folioLinks = {
		<ul>
			{ 
				for (f <- MainModel.folios) yield {
					<li>
						<i>Iliad</i>
						{ s"${f.textUrn.passageComponent}:" }
						{ 
							f.facsimileUrl match {
								case Some(u) => {
									{ facsLink(f).bind }
								}														
								case _ => {
									<!-- empty content -->
								}
							}
							
						}		

					</li>
				}
			}
		</ul>	
	}

	@dom
	def facsLink(f:MainModel.FolioReport) = {
		<span>
		<a href={ s"${f.facsimileUrl.get}" }>Facsimile.</a>
			<span class="otherViews">
				Other views: 
				<a href={ s"${MainModel.ictUrl.value}?urn=${f.imageUrn.get}" }>Passage in context.</a>
				Integrated CITE environment: 
				<a href={ s"${MainModel.appUrl.value}?urn=${f.imageUrn.get}" }>image</a>,
				<a href={ s"${MainModel.appUrl.value}?urn=${f.textUrn}" }>text</a>,
				<a href={ s"${MainModel.appUrl.value}?urn=${f.surfaceUrn.get}" }>folio</a>.
			</span>
		</span>
	}

	@dom
	def imageLink(u:Cite2Urn) = {
		<a href={ s"${MainModel.ictUrl}?${u}" }>This passage on the manuscript.</a>
	}



	@dom
	def footer = {
		<p><a href="http://www.homermultitext.org">The Homer Multitext</a>, Casey Dué and Mary Ebbott, edd. The Center for Hellenic Studies of Harvard University (2002–2018).
		CITE/CTS is ©2002–2018 Neel Smith and Christopher Blackwell. This implementation of the <a href="http://cite-architecture.github.io">CITE</a> data models was written by Neel Smith and Christopher Blackwell using <a href="https://www.scala-lang.org">Scala</a>, <a href="http://www.scala-js.org">Scala-JS</a>, and <a href="https://github.com/ThoughtWorksInc/Binding.scala">Binding.scala</a>. Licensed under the <a href="https://www.gnu.org/licenses/gpl-3.0.en.html">GPL 3.0</a>. Sourcecode on <a href="https://github.com/cite-architecture/hmtIndex">GitHub</a>. Copyright and licensing information for the default library is available <a href="https://raw.githubusercontent.com/Eumaeus/cts-demo-corpus/master/CEX-Files/LICENSE.markdown">here</a>. Report bugs by <a href="https://github.com/cite-architecture/CITE-App/issues">filing issues on GitHub.</a>
		</p>
	}


}
