package hubmodel.mvmtmodels

import hubmodel.DES.{Action, NOMADGraphSimulator, PedestrianPrediction}

class Energize(sim: NOMADGraphSimulator) extends Action {

  override def execute(): Unit = ???

  type A = Energize

  type B = NOMADGraphSimulator

override def deepCopy(simulator: PedestrianPrediction): Option[A] = ???

}
