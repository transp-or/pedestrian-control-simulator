package optimization.bruteforce

import com.typesafe.config.Config
import hubmodel.parseConfigFile
import myscala.math.stats.bootstrap.bootstrapMSE
import myscala.math.stats.{computeBoxPlotData, computeQuantile}
import myscala.output.SeqTuplesExtensions.SeqTuplesWriter
import optimization.bruteforce.explorations.{ComplianceVariation, RouteChoiceBetaExploration}

object ExploreRouteChoiceBeta extends App {

  // ******************************************************************************************
  //                    Read CLI arguments and process parameters file
  // ******************************************************************************************

  // Reads the file passed as argument
  val config: Config = parseConfigFile(args)

  val complianceIntervals = 0.1

  val analysis: RouteChoiceBetaExploration = new RouteChoiceBetaExploration(config, 0.1, 0.9, 0.1)

  analysis.runSimulations()

  def mean(data: Seq[Double]): Double = {
    data.sum / data.size
  }

  def median(data: Seq[Double]): Double = computeQuantile(50.0)(data).value.toDouble

  def quant(data: Seq[Double]): Double = computeQuantile(90.0)(data).value.toDouble

  val results = analysis.results
}
