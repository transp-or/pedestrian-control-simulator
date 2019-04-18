package optimization.simulation

import java.util.Collections

import SimulatedAnnealing.Factories.MinFunctionFactory
import SimulatedAnnealing.ContinuousProblem

object TestScala extends App {

  private val MiF_DSA = true
  val path = System.getProperty("user.dir") + "/tmp/"


  val dimension = 1
  //DSA
  val x_only1 = ContinuousProblem.problemInit(dimension, Collections.singletonList(-5.0), Collections.singletonList(5.0))
  val factory1 = new MinFunctionFactory
  val title1 = path + "DSA_MinFunction.txt"
  if (MiF_DSA) {
    ContinuousProblem.optimizationDSA(10e3, 0.002, 0.05, new java.util.ArrayList[java.lang.Object](x_only1), factory1, title1, false)
  }


}
