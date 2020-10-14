package optimization.ALNS

import java.nio.file.{Files, Path, Paths}
import myscala.math.stats._
import myscala.output.SeqTuplesExtensions._

import scala.jdk.CollectionConverters._

object AnalyseALNSIterations extends App {

  val dirWithIterations: String = "E:\\PhD\\hub-simulator\\results-to-keep\\piw\\results-amws-predictive\\ALNS_iterations\\"

  val dirContents = Files.newDirectoryStream(Paths.get(dirWithIterations))
  val files: Vector[Path] = dirContents.asScala.toVector.collect({ case f if Files.isRegularFile(f) && f.getFileName.toString.contains("NS_points") => f })
  dirContents.close()
  files.map(f => {
    val in = scala.io.Source.fromFile(dirWithIterations + f.getFileName.toString)
    val data = (for (line <- in.getLines.drop(1)) yield {
      val data = line.split(",").map(_.trim)
      (data.takeRight(6).map(_.toDouble).toVector)
    }).toVector.last
    in.close
    data
  }).transpose
    .zipWithIndex
    .map(w => {
      println(computeQuantiles(Vector(1,3,5,10,25,50,75,90,95, 97, 99))(w._1))
    println(computeBoxPlotData(w._1))
      (w._2, w._2, computeBoxPlotData(w._1).toCSV, 1.0)})
    .writeToCSV("test.csv", rowNames = None, columnNames = Some(Vector("pos", "name", "mean", "median", "lq", "uq", "lw", "uw", "outliersize", "boxplotwidth")))
}
