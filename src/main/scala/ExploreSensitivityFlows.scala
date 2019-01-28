import com.typesafe.config.Config
import hubmodel.parseConfigFile
import optimization.bruteforce.FlowSensitivity

object ExploreSensitivityFlows extends App {

  // ******************************************************************************************
  //                    Read CLI arguments and process parameters file
  // ******************************************************************************************

  // Reads the file passed as argument
  val config: Config = parseConfigFile(args)


  val flowRatioSensitivity: FlowSensitivity = new FlowSensitivity(config)
  flowRatioSensitivity.varyOpposingFlows(1.0/10.0, 1.5)

  val results = flowRatioSensitivity.processWrittenResults
  flowRatioSensitivity.drawResults(results)

  val resultsSplitOD = flowRatioSensitivity.processWrittenResultsSplitOD
  flowRatioSensitivity.drawResultsSplitOD(resultsSplitOD)

}