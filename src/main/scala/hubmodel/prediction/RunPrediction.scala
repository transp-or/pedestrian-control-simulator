package hubmodel.prediction

import hubmodel.DES.{Action, NOMADGraphSimulator, PedestrianPrediction, PedestrianSimulation}
import tools.Time

/** [[Action]] will runs the prediction on execution. The whole prediction is stored in the reference simulator
  * so the data can be used by the controller.
  *
  * @param sim
  */
class RunPrediction(sim: PedestrianSimulation, predHorizon: Time, predInterval: Time, densityUpdateInterval: Time) extends Action {

  type A = RunPrediction

  type B = NOMADGraphSimulator

override def deepCopy(simulator: PedestrianPrediction): Option[A] = None

  override def execute(): Unit = {

    // Creates the simulation to use for prediction
    val prediction = new PredictWithGroundTruth(sim, predHorizon, predInterval, densityUpdateInterval: Time)

    // Performs the prediction
    prediction.predict()

    // Stores the result from the prediction in the main simulator
    this.sim.updatePrediction(prediction)

  }

}
