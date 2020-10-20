package optimization.bruteforce

import java.io.{BufferedReader, FileWriter}

import com.typesafe.config.Config
import hubmodel.parseConfigFile
import myscala.math.stats.{computeBoxPlotData, computeQuantile}
import myscala.output.SeqTuplesExtensions.SeqTuplesWriter
import optimization.bruteforce.explorations.{ComplianceVariation, RouteChoiceBetaExploration}

object ExploreRouteChoiceBeta extends App {

  // ******************************************************************************************
  //                    Read CLI arguments and process parameters file
  // ******************************************************************************************

  // Reads the file passed as argument
  val config: Config = parseConfigFile(args)

  //val analysis: RouteChoiceBetaExploration = new RouteChoiceBetaExploration(config, 0.1, 0.9, 0.025)
  val analysis: RouteChoiceBetaExploration = new RouteChoiceBetaExploration(config, Vector(0.00001,0.000025,0.00005,0.000075,0.0001,0.00025,0.0005,0.00075,0.001,0.0025,0.005,0.0075,0.01,0.025,0.05,0.075,0.1))

  analysis.runSimulations()

  val bufferedSource = io.Source.fromFile("piw-route-choice-shortest-path_routes_usage.csv")
  val header = bufferedSource.getLines().iterator.next()
  val colsToLabels: Map[Int, String] = header.split(",").zipWithIndex.map(d => d._2 -> d._1).toMap
  val data: Map[String, Double] = {
    (for (line <- bufferedSource.getLines) yield {
      val cols = line.split(",").map(_.trim)
      // do whatever you want with the columns here
      cols(0) -> cols(1).toDouble
      //println(s"${cols(0)}|${cols(1)}|${cols(2)}|${cols(3)}")
    }).toMap
  }
  bufferedSource.close

  val results = analysis.results(data)
}
