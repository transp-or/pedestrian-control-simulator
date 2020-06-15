package hubmodel.prediction.state

import hubmodel.Position
import hubmodel.control.ControlDevices
import hubmodel.ped.PedestrianNOMAD
import hubmodel.prediction.AMWFlowsFromGroundTruth
import tools.Time
import tools.TimeNumeric.mkOrderingOps
import tools.cells.DensityMeasuredArea
import myscala.math.stats.computeQuantile

class StateGroundTruth(_population: Vector[(PedestrianNOMAD, Iterable[(Time, String, Position)])],
                       _controlDevices: ControlDevices) {

  val population = _population
  val controlDevices = _controlDevices
}


class StateGroundTruthPredicted(val startTime: Time,
                                val endTime: Time,
                                val interval: Time,
                       _population: Vector[PedestrianNOMAD],
                       _controlDevices: ControlDevices,
                       _criticalAreas: Map[String, DensityMeasuredArea]) {

  val intervals: Vector[Time] = this.startTime.value.to(this.endTime.value).by(this.interval.value).map(v => Time(v.toDouble)).toVector

  lazy val amwFlows: AMWFlowsFromGroundTruth = new AMWFlowsFromGroundTruth(_population, _controlDevices.amws.toVector, this.intervals)

  lazy val  densitiesInsideAreas: Map[String, Vector[(Time, Vector[Double])]] = this._criticalAreas.map(a => a._1 -> a._2.paxIndividualDensityHistory.toVector)

  val indicators: Map[String, Double] = {
    val populationMvmtIdxs = _population
      .map(ped => (intervals.indexWhere(_ > ped.entryTime), intervals.indexWhere(_ > ped.exitTime)))

    val inflow: Map[Int, Int] = populationMvmtIdxs.groupBy(_._1).view.mapValues(_.size).toMap
    val outflow: Map[Int, Int] = populationMvmtIdxs.groupBy(_._2).view.mapValues(_.size).toMap

    Map(
      "throughput" -> inflow.view.filterKeys(_ > 0).map(kv => kv._2 - outflow.getOrElse(kv._1, 0)).sum,
      "meanTT" -> _population.map(p => p.travelTime.value.toDouble).sum / _population.size,
      "density" -> _criticalAreas.toVector.flatMap(a => a._2.paxIndividualDensityHistory.map(d => d._1.value.toDouble * {if (d._2.isEmpty){0.0} else {computeQuantile(75)(d._2.toVector).value}})).sum
    )
  }



  }


