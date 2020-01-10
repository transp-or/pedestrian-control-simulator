package hubmodel.mgmt.state

import hubmodel.ped.PedestrianNOMAD

class CurrentStateGroundTruth[T <: PedestrianNOMAD](val population: Iterable[T]) extends CurrentState {

}
