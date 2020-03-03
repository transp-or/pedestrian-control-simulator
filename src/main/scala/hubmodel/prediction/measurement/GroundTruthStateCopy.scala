package hubmodel.prediction.measurement

import hubmodel.DES.{Action, NOMADGraphSimulator}
import hubmodel.P

class GroundTruthStateCopy extends Action {

  def execute(): Any = ???

  type A = GroundTruthStateCopy

  override def deepCopy(simulator: NOMADGraphSimulator): Option[A] = ???

}
