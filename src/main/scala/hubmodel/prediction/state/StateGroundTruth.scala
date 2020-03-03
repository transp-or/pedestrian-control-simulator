package hubmodel.prediction.state

import hubmodel.P
import hubmodel.control.ControlDevices
import hubmodel.ped.PedestrianNOMAD

class StateGroundTruth(_population: Vector[PedestrianNOMAD], _controlDevices: ControlDevices) {

  val population = _population
  val controlDevices = _controlDevices

}

