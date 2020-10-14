package hubmodel.prediction.state

import hubmodel.Position
import hubmodel.control.ControlDevices
import hubmodel.control.amw.MovingWalkway
import hubmodel.ped.PedestrianNOMAD
import hubmodel.prediction.{AMWFlowsFromGroundTruth, CongestionDataFromGroundTruth}
import tools.Time
import tools.TimeNumeric.mkOrderingOps
import tools.cells.DensityMeasuredArea
import myscala.math.stats.computeQuantile

class StateGroundTruth(_population: Vector[(PedestrianNOMAD, Iterable[(Time, String, Position)])],
                       _controlDevices: ControlDevices) {

  val population = _population
  val controlDevices = _controlDevices
}


class StateGroundTruthPredicted(val intervals: Vector[Time],
                                val amwFlows: AMWFlowsFromGroundTruth,
                                val densitiesInsideAreas: CongestionDataFromGroundTruth,
                                val indicators: Map[String, Double])


