package hubmodel.prediction

import hubmodel.DES.{Action, NOMADGraphSimulator, PedestrianSimulation}
import hubmodel.P
import hubmodel.ped.PedestrianSim

/** [[Action]] will runs the prediction on execution. The whole prediction is stored in the reference simulator
  * so the data can be used by the controller.
  *
  * @param sim
  * @tparam T
  */
class RunPrediction(sim: PedestrianSimulation) extends Action {

  type A = RunPrediction

  override def deepCopy(simulator: NOMADGraphSimulator): Option[A] = None

  override def execute(): Unit = {

    // Creates the simulation to use for prediction
    val prediction = new PredictWithGroundTruth(sim)

    // Performs the prediction
    prediction.predict()

    // Stores the result from the prediction in the main simulator
    this.sim.updatePrediction(prediction)

  }

}
