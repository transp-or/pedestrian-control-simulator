package optimization.bruteforce

import com.typesafe.config.Config
import hubmodel.{SimulatorParameters, createSimulation}
import hubmodel.ped.PedestrianNOMAD

import myscala.math.stats.ComputeStats

import scala.collection.GenIterable
import scala.collection.parallel.ForkJoinTaskSupport

case class ParameterModificationsCompliance(p: Double, i: Int) extends ParameterModifications(i)

class ComplianceVariation(complianceInterval: Double, c: Config) extends GridSearchNew[ParameterModificationsCompliance](c) {

  override val simulationRunsParameters: GenIterable[ParameterModificationsCompliance] = if (config.getBoolean("execution.parallel")) {
    val r = (for (i <- BigDecimal(1.0) to BigDecimal(0.5) by BigDecimal(-complianceInterval); k <- 1 to config.getInt("sim.nb_runs")) yield {ParameterModificationsCompliance(i.toDouble,k)}).par
    r.tasksupport = new ForkJoinTaskSupport(new scala.concurrent.forkjoin.ForkJoinPool(config.getInt("execution.threads")))
    r
  } else {
    for (i <- BigDecimal(1.0) to BigDecimal(0.5) by BigDecimal(-complianceInterval); k <- 1 to config.getInt("sim.nb_runs")) yield {ParameterModificationsCompliance(i.toDouble,k)}
  }

  def getParameters(paramMods: ParameterModificationsCompliance): SimulatorParameters = {

    val devices = defaultParameters._11.clone()

    (
      defaultParameters._1,
      defaultParameters._2,
      defaultParameters._3,
      defaultParameters._4,
      defaultParameters._5,
      defaultParameters._6,
      defaultParameters._7,
      defaultParameters._8.clone2AlternateGraphs(devices, paramMods.p),
      defaultParameters._9,
      defaultParameters._10,
      devices
    )
  }

  def getRunPrefix(paramMods: ParameterModificationsCompliance): String = { paramMods.p.toString + "_params_" }


  def processWrittenResults: Map[(Double), ((Int, Double, Double, Double, Double, Double), Iterable[Double])] = {

    groupResultsFiles("tt").map(ProcessTTFile1Parameter).
      flatMap(tup => tup._2.map(t => (tup._1, t._1._1, t._1._2, t._2))).
      groupBy(tup => tup._1).
      mapValues(v => (v.flatMap(_._4).stats, v.flatMap(_._4)))
  }
}
