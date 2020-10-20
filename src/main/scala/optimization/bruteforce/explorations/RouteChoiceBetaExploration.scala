package optimization.bruteforce.explorations

import com.typesafe.config.Config
import hubmodel.DES.{SimulationInputParameters, getAggregateFlows}
import hubmodel.demand.{PedestrianFlowFunction_New, PedestrianFlowPT_New, PedestrianFlow_New}
import hubmodel.results.{ResultsContainerReadNew, readResultsJson}
import optimization.bruteforce.parameters.ParameterModificationRouteChoice
import myscala.output.SeqTuplesExtensions.SeqTuplesWriter
import myscala.output.SeqOfSeqExtensions.SeqOfSeqWriter

class RouteChoiceBetaExploration(c: Config, lowerBound: Double, upperBound: Double, interval: Double = 0.1) extends GridSearchNew[ParameterModificationRouteChoice](c) {

  override val simulationRunsParameters: Vector[ParameterModificationRouteChoice] = {
    (for (i <- BigDecimal(lowerBound) to BigDecimal(upperBound) by BigDecimal(interval); k <- 1 to config.getInt("sim.nb_runs")) yield {
      ParameterModificationRouteChoice(i.toDouble)
    }).toVector
  }

  override def getParameters(paramMods: ParameterModificationRouteChoice): SimulationInputParameters = {

    val devices = defaultParameters.controlDevices.deepCopy

    defaultParameters.deepCopy(defaultParameters.graph.deepCopyChangeRouteChoiceBeta(devices, paramMods.beta), devices, None)
  }

  override def getRunPrefix(paramMods: ParameterModificationRouteChoice): String = {
    config.getString("output.output_prefix") + "_" + paramMods.beta.toString + "_params_"
  }

  override def getFlowMods(paramMods: ParameterModificationRouteChoice): (Iterable[PedestrianFlow_New], Iterable[PedestrianFlowPT_New], Iterable[PedestrianFlowFunction_New]) = {
    getAggregateFlows(config)
  }

  def results(refData: Map[String, Double] = Map()): Unit = {
    val parameters = (for (i <- BigDecimal(lowerBound) to BigDecimal(upperBound) by BigDecimal(interval)) yield {
      ParameterModificationRouteChoice(i.toDouble)
    }).toVector

    val data: Vector[Vector[(String, Int, Double)]] = parameters.map(p => {
      val r = readResultsJson(config.getString("output.dir"), getRunPrefix(p)).map(_.tt)
      val pop: Int = r.flatten.size
      r.flatMap(_.map(_.route.map(_.node).distinct.mkString("-")))
        .groupBy(s => s)
        .view
        .mapValues(_.size)
        .map(g => (g._1, g._2, g._2.toDouble/pop))
        .toVector
    })

    val rows = data.flatMap(d => d.map(_._1)).distinct.sorted
    (
      Vector(rows.map(r => refData.getOrElse(r, Double.NaN))) ++
        data.map(d => rows.map(r => d.find(_._1 == r).getOrElse((r, 0, Double.NaN))._3))
    ).writeToCSV(config.getString("output.output_prefix") + "_routes_usage" + ".csv", rowNames=Some(rows), columnNames=Some(Vector("route", "ref") ++ parameters.map(_.beta.toString)))
  }

}
