package hubmodel.mgmt.measurement

import hubmodel.DES.{Action, NOMADGraphSimulator}
import hubmodel.P

class GroundTruthStateCopy extends Action {

  def execute(): Any = ???

  type A = GroundTruthStateCopy

  override def deepCopy(simulator: NOMADGraphSimulator[P]): Option[A] = ???

}
