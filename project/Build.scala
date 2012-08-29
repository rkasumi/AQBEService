import sbt._
import Keys._
import PlayProject._

object ApplicationBuild extends Build {

    val appName         = "ADLParser"
    val appVersion      = "1.1"

    val appDependencies = Seq(
      // Add your project dependencies here,
    )

    val main = PlayProject(appName, appVersion, appDependencies, mainLang = SCALA).settings(
      resolvers += "tototoshi.github.com maven-repo/releases" at "http://tototoshi.github.com/maven-repo/releases",
      libraryDependencies ++= Seq(
        "com.github.tototoshi" %% "lift-json-play-module" % "0.1",
        "org.mongodb" %% "casbah" % "3.0.0-M2"
      )
    )

}
