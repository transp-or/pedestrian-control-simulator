package hubmodel.mvmtmodels

import hubmodel.DES.{Action, NOMADGraphSimulator}
import hubmodel.ped.PedestrianNOMAD

class Energize[T <: PedestrianNOMAD](sim: NOMADGraphSimulator[T]) extends Action {

  override def execute(): Unit = ???

}
