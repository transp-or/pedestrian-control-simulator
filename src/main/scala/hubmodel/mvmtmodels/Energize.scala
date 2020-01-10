package hubmodel.mvmtmodels

import hubmodel.DES.{Action, NOMADGraphSimulator}
import hubmodel.P
import hubmodel.ped.PedestrianNOMAD

class Energize[T <: PedestrianNOMAD](sim: NOMADGraphSimulator[T]) extends Action {

  override def execute(): Unit = ???

  type A = Energize[P]

  override def deepCopy(simulator: NOMADGraphSimulator[P]): Option[A] = ???

}
