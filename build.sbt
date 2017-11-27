
name := "hub-simulation"
organization := "transpor.molyneaux"
version := "1.0-SNAPSHOT"
scalaVersion := "2.11.8"
fork in run := true

javaOptions in run ++= Seq(
    "-Xms512M", "-Xmx14G", "-XX:+UseConcMarkSweepGC"
)

libraryDependencies ++= Seq(
    "com.typesafe.play" %% "play-json" % "2.6.5",
    "org.scalanlp" %% "breeze" % "0.12",
    "org.scalanlp" %% "breeze-natives" % "0.12",
    "org.scala-lang" % "scala-swing" % "2.10+",
    "org.jgrapht" % "jgrapht-core" % "1.0.1",
    "transpor.molyneaux" %% "scala-custom" % "1.1.6.2",
    "com.github.scopt" %% "scopt" % "3.6.0",
    "org.jcodec" % "jcodec-javase" % "0.2.0",
    "com.typesafe.scala-logging" %% "scala-logging" % "3.1.0",
    "com.typesafe" % "config" % "1.3.1",
    "ch.qos.logback" % "logback-classic" % "1.2.3",
    "org.scalactic" %% "scalactic" % "3.0.1",
    "transpor.tools" %% "power-voronoi" % "SNAPSHOT"
)

resolvers ++= Seq(
  "Typesafe Repo" at "http://repo.typesafe.com/typesafe/releases/")

// https://stackoverflow.com/questions/28459333/how-to-build-an-uber-jar-fat-jar-using-sbt-within-intellij-idea
// META-INF discarding

mainClass in assembly := Some("makePictures")
