package optimization.bruteforce

import com.typesafe.config.Config
import hubmodel.demand.{PedestrianFlowFunction_New, PedestrianFlowPT_New, PedestrianFlow_New}
import hubmodel.{SimulatorParameters}
import hubmodel.DES.getFlows
import scala.collection.GenIterable
import scala.collection.parallel.ForkJoinTaskSupport


class ComplianceVariation(complianceInterval: Double, c: Config, upperBoundCompliance: Double = 0.5) extends GridSearchNew[ParameterModificationsCompliance](c) {

  override val simulationRunsParameters: GenIterable[ParameterModificationsCompliance] = if (config.getBoolean("execution.parallel")) {
    val r = (for (i <- BigDecimal(0.0) to BigDecimal(upperBoundCompliance) by BigDecimal(complianceInterval); k <- 1 to config.getInt("sim.nb_runs")) yield {
      ParameterModificationsCompliance(i.toDouble)
    }).par
    r.tasksupport = new ForkJoinTaskSupport(new scala.concurrent.forkjoin.ForkJoinPool(config.getInt("execution.threads")))
    r
  } else {
    for (i <- BigDecimal(0.0) to BigDecimal(upperBoundCompliance) by BigDecimal(complianceInterval); k <- 1 to config.getInt("sim.nb_runs")) yield {
      ParameterModificationsCompliance(i.toDouble)
    }
  }

  def getParameters(paramMods: ParameterModificationsCompliance): SimulatorParameters = {

    val devices = defaultParameters._11.deepCopy

    (
      defaultParameters._1,
      defaultParameters._2,
      defaultParameters._3,
      defaultParameters._4,
      defaultParameters._5,
      defaultParameters._6,
      defaultParameters._7,
      defaultParameters._8.deepCopy2AlternateGraphs(devices, paramMods.complianceRate),
      defaultParameters._9,
      defaultParameters._10,
      devices
    )
  }

  def getRunPrefix(paramMods: ParameterModificationsCompliance): String = {
    paramMods.complianceRate.toString + "_params_"
  }

  def getFlowMods(paramMods: ParameterModificationsCompliance): (Iterable[PedestrianFlow_New], Iterable[PedestrianFlowPT_New], Iterable[PedestrianFlowFunction_New]) = getFlows(config)


  def processWrittenResults(func: Seq[Double] => Double): Map[(Double), (Iterable[Double], Iterable[Iterable[Double]])] = {

    groupResultsFiles("tt").map(ProcessTTFile1Parameter).
      flatMap(tup => tup._2.map(t => (tup._1, t._1._1, t._1._2, t._2))).
      groupBy(tup => tup._1).
      mapValues(v => (v.map(d => func(d._4)), v.map(_._4)))
  }

  def processWrittenResultsByOD(func: Seq[Double] => Double): Map[(Double), (Map[(String, String),Iterable[Double]], Iterable[Iterable[Double]])] = {
    groupResultsFiles("tt").map(ProcessTTFile1Parameter).
      flatMap(tup => tup._2.map(t => (tup._1, t._1._1, t._1._2, t._2))).
      groupBy(tup => tup._1).
      mapValues(v => {
        (
          v.groupBy(p => (p._2, p._3)).map(r => r._1 -> r._2.map(p => func(p._4))),
          v.map(_._4)
        )
      })
  }
}
