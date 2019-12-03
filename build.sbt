name := "hub-simulator"
organization := "ch.epfl.transpor.pedestrians"
version := "1.0-SNAPSHOT"
scalaVersion := "2.13.1"
fork in run := true

javaOptions in run ++= Seq(
  "-Xms1G", "-Xmx12G", "-XX:+UseConcMarkSweepGC"
)

// Dependencies taken from maven
libraryDependencies ++= Seq(
  "com.typesafe.play" %% "play-json" % "2.7.4",
  "org.jgrapht" % "jgrapht-core" % "1.0.1",
  "com.github.scopt" %% "scopt" % "3.7.1",
  "org.jcodec" % "jcodec-javase" % "0.2.0",
  "com.typesafe" % "config" % "1.3.1",
  "ch.qos.logback" % "logback-classic" % "1.2.3",
  "com.typesafe.scala-logging" %% "scala-logging" % "3.9.2",
  "org.scalactic" %% "scalactic" % "3.0.8",
  "transpor.tools" % "power-voronoi" % "1.0",
  "com.github.NicholasMolyneaux" %% "scala-custom" % "1.3.3",
  "transpor.tools" % "dxf-parser" % "1.0",
  "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.2",
  "org.apache.commons" % "commons-lang3" % "3.8",
  "org.apache.commons" % "commons-math3" % "3.6",
  "org.scala-lang.modules" %% "scala-parallel-collections" % "0.2.0"
)

// Dependencies installed manually with sbt
libraryDependencies ++= Seq(
  "transpor.tools" % "power-voronoi" % "1.0",
  "transpor.tools" % "dxf-parser" % "1.0"
)

// Extra places to look for libraries  (useful for the scala-custom, avoids waiting for new versions to be copied to maven central
//resolvers += "Local Maven Repository" at "file://"+Path.userHome.absolutePath+"/.m2/repository"
libraryDependencies += "transpor.student-projects" % "hub-model-optimization" % "1.0-SNAPSHOT"

libraryDependencies ++= {
  CrossVersion.partialVersion(scalaVersion.value) match {
    case Some((2, major)) if major >= 13 =>
      Seq("org.scala-lang.modules" %% "scala-parallel-collections" % "0.2.0")
    case _ =>
      Seq()
  }
}

// Extra places to look for libraries  (useful for the scala-custom, avoids waiting for new versions to be copied to maven central
resolvers ++= Seq(
  "Typesafe Repo" at "http://repo.typesafe.com/typesafe/releases/",
  "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/releases/"
)

// euh.. I think is obsolete
// https://stackoverflow.com/questions/28459333/how-to-build-an-uber-jar-fat-jar-using-sbt-within-intellij-idea
// META-INF discarding

/*
 Custom sbt command to create a fat jar to distribute to run the hub model for TRANS-FORM.
 The input files for Den Haag case study are also included in the folder.
 */
mainClass in(Compile, packageBin) := Some("RunSimulation")

// data folders where to copy files form
lazy val dataFolders = Array("den-haag")

// extra set of files to copy
lazy val files = Array()

// config files to copy to the distributed folder
lazy val confFiles = Array("den-haag-integration.conf")

// Custom key
lazy val distribution = taskKey[Unit]("Copies all the required files and builds a standalone jar to distribute.")

// Actions to perform when invoking the "distribution" task.
distribution := {
  IO.copyFile(assembly.value.getAbsoluteFile, baseDirectory.value.getAbsoluteFile / "distribution/hub-model.jar") // gets the jar
  dataFolders.foreach(df => IO.copyDirectory(baseDirectory.value / df, baseDirectory.value / "distribution" / df)) // copies everything from the data folde to the distributed folder
  confFiles.foreach(f => IO.copyFile(baseDirectory.value / "resources/simulation" / f, baseDirectory.value / "distribution" / f)) // copies the configuration files
}

