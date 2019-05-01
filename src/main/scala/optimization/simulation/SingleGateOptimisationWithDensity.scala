package optimization.simulation

import java.io.FileWriter
import java.{lang, util}

import SimulatedAnnealing.ContinuousProblem
import com.typesafe.config.Config

class SingleGateOptimisationWithDensity(val config: Config, ID: String, params: java.util.ArrayList[java.lang.Double]) extends ContinuousProblem(params) {

  private val curr_x1 = super.getXs.get(0).asInstanceOf[Double]
  private val curr_x2 = super.getXs.get(1).asInstanceOf[Double]
  private val curr_x3 = super.getXs.get(2).asInstanceOf[Double]
  private val curr_x4 = super.getXs.get(3).asInstanceOf[Double]

  def getObjectiveFunction(x: util.ArrayList[lang.Double]): Double = {

    val res = runGatingSingleFunction(config)(x.get(0),x.get(1),x.get(2),x.get(3))

    val fw = new FileWriter(config.getString("output.dir") + "/SO_gating_KPIs_" + ID + ".csv", true)
    fw.write(res.getOrElse("allPeds", Double.NaN) + "," + res.getOrElse("pedsThroughGate", Double.NaN) + "," + res.getOrElse("pedsWithoutgates", Double.NaN) + "\n")
    fw.close()

    res.getOrElse("allPeds", Double.MaxValue)
  }

  override def pbWithGoodType(newX: util.ArrayList[lang.Double]) = new SingleGateOptimisationWithDensity(config, ID, newX)

  override def printSolution(s: String, currObjective: Double): Unit = {
    System.out.println(s)
    System.out.println("For x1 = " + curr_x1 + ", x2 = " + curr_x2 + ", x3 = " + curr_x3 + ", x4 = " + curr_x4 + " ---> y = " + currObjective)
  }
}