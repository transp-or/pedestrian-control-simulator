package hubmodel.mgmt.state

import hubmodel.mgmt.ControlDevices
import hubmodel.ped.PedestrianNOMAD

class CurrentStateGroundTruth[T <: PedestrianNOMAD](val population: Vector[T], val controlDevices: ControlDevices) extends CurrentState {

}
