package hubmodel.mvmtmodels

import hubmodel.DES.{Action, NOMADGraphSimulator}
import hubmodel.P
import hubmodel.ped.PedestrianNOMAD

class Energize(sim: NOMADGraphSimulator) extends Action {

  override def execute(): Unit = ???

  type A = Energize

  override def deepCopy(simulator: NOMADGraphSimulator): Option[A] = ???

}
