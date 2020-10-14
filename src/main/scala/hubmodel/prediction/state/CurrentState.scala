package hubmodel.prediction.state

import hubmodel.control.ControlDevices
import hubmodel.ped.PedestrianNOMAD

abstract class CurrentState {

  def population: Vector[PedestrianNOMAD]

  def controlDevices: ControlDevices

}