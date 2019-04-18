package optimization

import java.io.FileWriter
import java.util
import java.util.{Arrays, Collections}

import SimulatedAnnealing.ContinuousProblem
import com.typesafe.config.Config
import hubmodel.parseConfigFile
import optimization.simulation.SingleGateOptimizationFactory
import org.apache.commons.lang3.RandomStringUtils

object OptimizeGatingStrategy extends App {

  // ******************************************************************************************
  //                    Read CLI arguments and process parameters file
  // ******************************************************************************************

  // Reads the file passed as argument
  val config: Config = parseConfigFile(args)


  // ******************************************************************************************
  //                    Optimization of the control strategy
  // ******************************************************************************************

  val dimension = 4
  val ID: String = RandomStringUtils.randomAlphabetic(1) + RandomStringUtils.randomAlphanumeric(8)

  val fw = new FileWriter(ID + "_SO_gating_KPIs.csv", true)
  fw.write("allPeds,pedsThroughGate,pedsWithoutgates\n")
  fw.close()

  val x_only1 =   ContinuousProblem.problemInit(dimension, util.Arrays.asList(2.0, -2.0, -3.0, 1.0), util.Arrays.asList(6.0 ,2.0 , 0.0, 5.0))
  val factory1 = new SingleGateOptimizationFactory(config, ID)
  val title1 = "/home/nicholas/PhD/code/hub-model-optimization/" + ID + "_DSA_results.txt"
  ContinuousProblem.optimizationDSA(10e3, 0.002, 0.05, new java.util.ArrayList[java.lang.Object](x_only1), factory1, title1, false)
}