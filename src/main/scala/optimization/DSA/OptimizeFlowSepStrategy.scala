package optimization.DSA

import java.io.FileWriter
import java.util

import SimulatedAnnealing.ContinuousProblem
import com.typesafe.config.Config
import hubmodel.parseConfigFile
import org.apache.commons.lang3.RandomStringUtils

object OptimizeFlowSepStrategy extends App {

  // ******************************************************************************************
  //                    Read CLI arguments and process parameters file
  // ******************************************************************************************

  // Reads the file passed as argument
  val config: Config = parseConfigFile(args)


  // ******************************************************************************************
  //                    Optimization of the control strategy
  // ******************************************************************************************

  val dimension = 3
  val ID: String = RandomStringUtils.randomAlphabetic(1) + RandomStringUtils.randomAlphanumeric(8)

  val fw = new FileWriter(config.getString("output.dir") + "/SO_flowSep_KPIs_" + ID + ".csv", true)
  fw.write("allPeds\n")
  fw.close()

  val x_only1 = ContinuousProblem.problemInit(dimension, util.Arrays.asList(0.5, 0.5, -3.0), util.Arrays.asList(2.0, 2.0, -0.25))
  val factory1 = new FlowSepOptimizationFactory(config, ID)
  val title1 = ID + "_DSA_results.txt"
  ContinuousProblem.optimizationDSA(10e3, 0.002, new java.util.ArrayList[java.lang.Object](x_only1), factory1, title1, false, 0.05)
}
