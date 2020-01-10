package hubmodel.route.Guo2011

import hubmodel.DES.{Action, NOMADGraphSimulator, PedestrianDES}
import hubmodel.ped.PedestrianNOMAD
import hubmodel.P

class UpdateRoutesGuo[T <: PedestrianNOMAD](sim: PedestrianDES[T]) extends Action {


  override def execute(): Unit = ???

  type A = UpdateRoutesGuo[P]

  override def deepCopy(simulator: NOMADGraphSimulator[P]): Option[A] = {???}

}
