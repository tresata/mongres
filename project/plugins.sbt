// Comment to get more information during initialization
logLevel := Level.Warn

// The Typesafe repository 
resolvers += "Typesafe repository" at "http://repo.typesafe.com/typesafe/releases/"

// Maven repo
// resolvers += "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/groups/scala-tools"

// Dependencies
// libraryDependencies += "com.mongodb.casbah" %% "casbah" % "2.1.5-1"

// Use the Play sbt plugin for Play projects
addSbtPlugin("play" % "sbt-plugin" % "2.0.4")