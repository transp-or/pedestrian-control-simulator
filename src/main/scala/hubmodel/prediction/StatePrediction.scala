package hubmodel.prediction

import hubmodel.prediction.state.StateGroundTruth

trait StatePrediction {

  def getPredictedStateData: StateGroundTruth

  protected def predict()

}
