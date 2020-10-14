package optimization.bruteforce

import com.typesafe.config.Config
import hubmodel.DES.{SimulationInputParameters, getAggregateFlows}
import hubmodel.SimulatorParameters
import hubmodel.demand.{PedestrianFlowFunction_New, PedestrianFlowPT_New, PedestrianFlow_New}

import scala.collection.GenIterable
import scala.collection.parallel.CollectionConverters._
import scala.collection.parallel.ForkJoinTaskSupport

class ComplianceVariation(complianceInterval: Double, c: Config, upperBoundCompliance: Double = 0.5) extends GridSearchNew[ParameterModificationsCompliance](c) {

  override val simulationRunsParameters: Vector[ParameterModificationsCompliance] = {/*if (config.getBoolean("execution.parallel")) {

    val r = (for (i <- BigDecimal(0.0) to BigDecimal(upperBoundCompliance) by BigDecimal(complianceInterval); k <- 1 to config.getInt("sim.nb_runs")) yield {
      ParameterModificationsCompliance(i.toDouble)
    }).par
    r.tasksupport = new ForkJoinTaskSupport(new java.util.concurrent.ForkJoinPool(config.getInt("execution.threads")))
    r
  } else {*/
    (for (i <- BigDecimal(0.0) to BigDecimal(upperBoundCompliance) by BigDecimal(complianceInterval); k <- 1 to config.getInt("sim.nb_runs")) yield {
      ParameterModificationsCompliance(i.toDouble)
    }).toVector
  }


  def getParameters(paramMods: ParameterModificationsCompliance): SimulationInputParameters = {

    val devices = defaultParameters.controlDevices.deepCopy

    defaultParameters.deepCopy(defaultParameters.graph.deepCopy2AlternateGraphs(devices, paramMods.complianceRate), devices, None)

  }

  def getRunPrefix(paramMods: ParameterModificationsCompliance): String = {
    paramMods.complianceRate.toString + "_params_"
  }

  def getFlowMods(paramMods: ParameterModificationsCompliance): (Iterable[PedestrianFlow_New], Iterable[PedestrianFlowPT_New], Iterable[PedestrianFlowFunction_New]) = getAggregateFlows(config)


  def processWrittenResults(func: Seq[Double] => Double): Map[(Double), (Iterable[Double], Iterable[Iterable[Double]])] = {

    groupResultsFiles("tt").map(ProcessTTFile1Parameter).
      flatMap(tup => tup._2.map(t => (tup._1, t._1._1, t._1._2, t._2))).
      groupBy(tup => tup._1).view
      .mapValues(v => (v.map(d => func(d._4)), v.map(_._4))).toMap
  }

  def processWrittenResultsByOD(func: Seq[Double] => Double): Map[(Double), (Map[(String, String), Iterable[Double]], Iterable[Iterable[Double]])] = {
    groupResultsFiles("tt").map(ProcessTTFile1Parameter).
      flatMap(tup => tup._2.map(t => (tup._1, t._1._1, t._1._2, t._2))).
      groupBy(tup => tup._1).view
      .mapValues(v => {
        (
          v.groupBy(p => (p._2, p._3)).map(r => r._1 -> r._2.map(p => func(p._4))),
          v.map(_._4)
        )
      }).toMap
  }
}
