package hubmodel.mgmt.prediction

import hubmodel.mgmt.state.CurrentState

trait Prediction {

  protected def getStateData: CurrentState

  protected def predict()

}
