package hubmodel.route.Guo2011

import hubmodel.DES.{Action, NOMADGraphSimulator, PedestrianDES, PedestrianPrediction}

class UpdateRoutesGuo(sim: PedestrianDES) extends Action {


  override def execute(): Unit = ???

  type A = UpdateRoutesGuo

  type B = NOMADGraphSimulator

override def deepCopy(simulator: PedestrianPrediction): Option[A] = {???}

}
