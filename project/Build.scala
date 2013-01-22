import sbt._
import Keys._
import PlayProject._

object ApplicationBuild extends Build {

    val appName         = "mongo-rest"
    val appVersion      = "1.0-SNAPSHOT"

    val appDependencies = Seq(
      // Add your project dependencies here,
        "com.mongodb.casbah" %% "casbah" % "2.1.5-1"
    )

    val main = PlayProject(appName, appVersion, appDependencies, mainLang = SCALA).settings(
      // Add your own project settings here 
        resolvers += "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/groups/scala-tools"
    )

}
