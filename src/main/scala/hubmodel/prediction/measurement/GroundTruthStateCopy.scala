package hubmodel.prediction.measurement

import hubmodel.DES.{Action, NOMADGraphSimulator, PedestrianPrediction}

class GroundTruthStateCopy extends Action {

  def execute(): Any = ???

  type A = GroundTruthStateCopy

  type B = NOMADGraphSimulator

override def deepCopy(simulator: PedestrianPrediction): Option[A] = ???

}
