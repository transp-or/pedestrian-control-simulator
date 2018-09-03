name := "hub-simulator"
organization := "ch.epfl.transpor.pedestrians"
version := "1.0-SNAPSHOT"
scalaVersion := "2.12.6"
fork in run := true

javaOptions in run ++= Seq(
  "-Xms1G", "-Xmx120G", "-XX:+UseConcMarkSweepGC"
)

libraryDependencies ++= Seq(
  "com.typesafe.play" %% "play-json" % "2.6.5",
  "org.scalanlp" %% "breeze" % "0.13",
  "org.scalanlp" %% "breeze-natives" % "0.13",
  "org.jgrapht" % "jgrapht-core" % "1.0.1",
  "com.github.scopt" %% "scopt" % "3.6.0",
  "org.jcodec" % "jcodec-javase" % "0.2.0",
  "com.typesafe" % "config" % "1.3.1",
  "ch.qos.logback" % "logback-classic" % "1.2.3",
  "com.typesafe.scala-logging" %% "scala-logging" % "3.7.2",
  "org.scalactic" %% "scalactic" % "3.0.1",
  "transpor.tools" % "power-voronoi" % "1.0",
  "com.github.NicholasMolyneaux" %% "scala-custom" % "1.1.1",
  "transpor.tools" % "dxf-parser" % "1.0",
  "nl.tudelft.pedestrians" % "nomad" % "1.0",
  "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.1",
  "org.apache.commons" % "commons-lang3" % "3.8"
)

//resolvers += Opts.resolver.sonatypeReleases // add if needed

// https://mvnrepository.com/artifact/org.j3d/aviatrix3d
libraryDependencies += "org.j3d" % "aviatrix3d" % "3.0.0" pomOnly()
libraryDependencies += "javax.vecmath" % "vecmath" % "1.5.2"


resolvers ++= Seq(
  "Typesafe Repo" at "http://repo.typesafe.com/typesafe/releases/",
  "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/releases/"
)

// https://stackoverflow.com/questions/28459333/how-to-build-an-uber-jar-fat-jar-using-sbt-within-intellij-idea
// META-INF discarding

mainClass in(Compile, packageBin) := Some("RunSimulation")

lazy val dataFolders = Array("toy-integration-test")

lazy val distribution = taskKey[Unit]("Copies all the required files and builds a standalone jar to distribute.")
distribution := {
  IO.copyFile(assembly.value.getAbsoluteFile, baseDirectory.value.getAbsoluteFile / "distribution/hub-model.jar")
  dataFolders.foreach(df => IO.copyDirectory(baseDirectory.value / df, baseDirectory.value / "distribution" / df))
  sourceDirectory.value / "main" / "resources" listFiles() filter (f => f.getAbsolutePath.takeRight(9) == "test.conf") foreach (f => IO.copyFile(f, baseDirectory.value / "distribution" / f.getName))
}

