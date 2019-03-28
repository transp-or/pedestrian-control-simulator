package optimization.simulation

import java.util

import SimulatedAnnealing.Factories.MinFunctionFactory
import SimulatedAnnealing.Optimization
import SimulatedAnnealing._MinFunction.MinFunction

object TestScala extends App {

  private val MiF_DSA = true
  val path = System.getProperty("user.dir") + "/tmp/"


  val range1 = MinFunction.problemInit
  val factory1 = new MinFunctionFactory(range1)
  val title1 = path + "DSA_MinFunction.txt"
  if (MiF_DSA) Optimization.optimizationDSA(10e3, 0.002, 0.05, 1, new util.ArrayList[AnyRef](range1), factory1, title1) //*/

}
