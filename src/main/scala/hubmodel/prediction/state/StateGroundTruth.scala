package hubmodel.prediction.state

import hubmodel.control.ControlDevices
import hubmodel.ped.PedestrianNOMAD
import hubmodel.prediction.AMWFlowsFromGroundTruth
import tools.Time
import tools.cells.DensityMeasuredArea

class StateGroundTruth(_population: Vector[PedestrianNOMAD],
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

  val population = _population
  val controlDevices = _controlDevices
  val criticalAreas = _criticalAreas

  val amwFlows: AMWFlowsFromGroundTruth = new AMWFlowsFromGroundTruth(this.population, this.controlDevices.amws.toVector, this.intervals)

  }


