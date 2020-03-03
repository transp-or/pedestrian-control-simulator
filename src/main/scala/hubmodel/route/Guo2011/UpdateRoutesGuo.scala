package hubmodel.route.Guo2011

import hubmodel.DES.{Action, NOMADGraphSimulator, PedestrianDES}
import hubmodel.ped.PedestrianNOMAD
import hubmodel.P

class UpdateRoutesGuo(sim: PedestrianDES) extends Action {


  override def execute(): Unit = ???

  type A = UpdateRoutesGuo

  override def deepCopy(simulator: NOMADGraphSimulator): Option[A] = {???}

}
