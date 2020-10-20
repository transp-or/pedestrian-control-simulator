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

  val analysis: RouteChoiceBetaExploration = new RouteChoiceBetaExploration(config, 0.1, 0.5, 0.025)

  analysis.runSimulations()

  val results = analysis.results
}
