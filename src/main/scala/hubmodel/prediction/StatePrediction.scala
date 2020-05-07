package hubmodel.prediction

import hubmodel.control.{ControlDeviceData, ControlDevicePolicy}
import hubmodel.prediction.state.{StateGroundTruth, StateGroundTruthPredicted}
import tools.Time

trait StatePrediction {

  def getRealisedControlData: Tuple1[Map[String, Double]]

  def getPredictedStateData: Vector[StateGroundTruthPredicted]
  def computeObjectives: Map[String, Vector[Double]]

  def predict(): Unit
  def predict(decisionVariables: Vector[ControlDevicePolicy], controlEvents: Vector[ControlDeviceData]): Unit

  val predictionStartTime: Time
  val predictionEndTime: Time
  val predictionInterval: Time

  val replications: Int

}
