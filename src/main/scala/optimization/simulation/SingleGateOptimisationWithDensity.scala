package optimization.simulation

import java.io.FileWriter
import java.{lang, util}

import SimulatedAnnealing.ContinuousProblem
import com.typesafe.config.Config

import scala.io.Source

class SingleGateOptimisationWithDensity(val config: Config, ID: String, params: java.util.ArrayList[java.lang.Double]) extends ContinuousProblem(params) {

  private val curr_x1 = super.getXs.get(0).asInstanceOf[Double]
  private val curr_x2 = super.getXs.get(1).asInstanceOf[Double]
  private val curr_x3 = super.getXs.get(2).asInstanceOf[Double]
  private val curr_x4 = super.getXs.get(3).asInstanceOf[Double]

  def getObjectiveFunction(x: util.ArrayList[lang.Double], nbrReplications: Int, newSimulation: Boolean): Double = {

    val simDirToUse: Option[String] = if (!newSimulation) {
      Some(Source.fromFile(config.getString("output.dir") + "/" + ID + "_previous-simulation-dir.txt").getLines.mkString)
    } else {
      None
    }

    if (simDirToUse.isDefined && simDirToUse.get != "" || simDirToUse.isEmpty) {
      val (simDir: String, res: Map[String, Double]) = runGatingSingleFunction(config, nbrReplications = Some(nbrReplications), simDir = simDirToUse)(x.get(0), x.get(1), x.get(2), x.get(3))

      val fw = new FileWriter(config.getString("output.dir") + "/SO_gating_KPIs_" + ID + ".csv", true)
      fw.write(
        res.getOrElse("allPedsTTmedmed", Double.NaN) + "," +
          res.getOrElse("allPedsTTvarmed", Double.NaN) + "," +
          res.getOrElse("allPedsSize", Double.NaN) + "," +
          res.getOrElse("withGatesTTmedmed", Double.NaN) + "," +
          res.getOrElse("withoutGatesTTmedmed", Double.NaN) + "," +
          res.getOrElse("withGatesTTvarmed", Double.NaN) + "," +
          res.getOrElse("withoutGatesTTvarmed", Double.NaN) + "," +
          res.getOrElse("allPedsTTmed75quant", Double.NaN) + "," +
          res.getOrElse("withGatesTTmed75quant", Double.NaN) + "," +
          res.getOrElse("withoutGatesTTmed75quant", Double.NaN) + "," +
          res.getOrElse("allPedsTT75quantmed", Double.NaN) + "," +
          res.getOrElse("indDens75quantmed", Double.NaN) + "," +
          res.getOrElse("indDens90quantmed", Double.NaN) + "," +
          res.getOrElse("allPedsTTzones75quantmed", Double.NaN) + "," +
          res.getOrElse("combined-allPedsTT75quantmed-allPedsTTzones75quantmed", Double.NaN) +
          "\n"
      )
      fw.close()

      val simDirFile = new FileWriter(config.getString("output.dir") + "/" + ID + "_previous-simulation-dir.txt", false)
      simDirFile.write(simDir)
      simDirFile.close()

      res.getOrElse("combined-allPedsTT75quantmed-allPedsTTzones75quantmed", Double.MaxValue)
    } else {
      Double.MaxValue
    }
  }

  override def pbWithGoodType(newX: util.ArrayList[lang.Double]) = new SingleGateOptimisationWithDensity(config, ID, newX)

  override def printSolution(s: String, currObjective: Double): Unit = {
    System.out.println(s)
    System.out.println("For x1 = " + curr_x1 + ", x2 = " + curr_x2 + ", x3 = " + curr_x3 + ", x4 = " + curr_x4 + " ---> y = " + currObjective)
  }
}
