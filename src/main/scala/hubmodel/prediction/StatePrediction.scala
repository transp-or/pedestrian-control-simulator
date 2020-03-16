package hubmodel.prediction

import hubmodel.control.ControlDevicePolicy
import hubmodel.prediction.state.{StateGroundTruth, StateGroundTruthPredicted}
import tools.Time

trait StatePrediction {

  def getPredictedStateData: StateGroundTruthPredicted
  def computeObjectives: Map[String, Double]

  def predict(): Unit
  def predict(decisionVariables: Iterable[ControlDevicePolicy]): Unit

  val predictionStartTime: Time
  val predictionEndTime: Time
  val predictionInterval: Time

}
