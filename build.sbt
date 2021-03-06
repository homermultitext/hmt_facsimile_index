enablePlugins(ScalaJSPlugin, BuildInfoPlugin)

name := "hmtIndex"

version := "2.0.0"

scalaVersion := "2.12.4"

resolvers += Resolver.jcenterRepo
resolvers += Resolver.bintrayRepo("neelsmith", "maven")
resolvers += sbt.Resolver.bintrayRepo("denigma", "denigma-releases")

val circeVersion = "0.10.0"

libraryDependencies ++= Seq(
  "org.scala-js" %% "scalajs-stubs" % scalaJSVersion % "provided",
  "org.scala-js" %%% "scalajs-dom" % "0.9.5",
  "io.monix" %%% "monix" % "2.3.0",
  "edu.holycross.shot.cite" %%% "xcite" % "4.2.0",
  "edu.holycross.shot" %%% "ohco2" % "10.18.2",
  "edu.holycross.shot" %%% "scm" % "7.2.0",
  "edu.holycross.shot" %%% "greek" % "1.4.0",
  "edu.holycross.shot" %%% "citeobj" % "7.4.0",
  "edu.holycross.shot" %%% "citerelations" % "2.6.0",
  "edu.holycross.shot" %%% "citebinaryimage" % "3.1.1",
  "edu.holycross.shot" %%% "citejson" % "3.0.0",
  "edu.holycross.shot" %%% "dse" % "7.0.0",
  "com.thoughtworks.binding" %%% "dom" % "latest.version"
)

libraryDependencies ++= Seq(
  "io.circe" %%% "circe-core",
  "io.circe" %%% "circe-generic",
  "io.circe" %%% "circe-optics",
  "io.circe" %%% "circe-parser"
).map(_ % circeVersion)




addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full)

//scalacOptions += "-P:scalajs:suppressExportDeprecations"
//scalacOptions += "-P:scalajs:suppressMissingJSGlobalDeprecations"
scalacOptions += "-unchecked"
scalacOptions += "-deprecation"


import scala.io.Source
import java.io.PrintWriter

buildInfoKeys := Seq[BuildInfoKey](name, version, scalaVersion, sbtVersion)
buildInfoPackage := "hmtIndex"
