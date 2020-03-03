package hubmodel.prediction

import hubmodel.P
import hubmodel.ped.PedestrianNOMAD
import hubmodel.prediction.state.{CurrentState, StateGroundTruth}

trait StatePrediction {

  def getPredictedStateData: StateGroundTruth

  protected def predict()

}
