package hubmodel.mgmt.prediction

import hubmodel.DES.{Action, NOMADGraphSimulator}
import hubmodel.mgmt.state.{CurrentState, CurrentStateGroundTruth}
import hubmodel.ped.PedestrianNOMAD

class PredictWithGroundTruth[T <: PedestrianNOMAD](sim: NOMADGraphSimulator[T]) extends Action with Prediction {

  private lazy val predictionSimulator: NOMADGraphSimulator[T] = ???

  /** Collect the state data from the ground truth simulator
    *
    * @return
    */
  protected def getStateData: CurrentStateGroundTruth[PedestrianNOMAD] = {
    new CurrentStateGroundTruth(sim.population.map(p => p.copyState(sim.currentTime, false)))
  }

  protected def predict(): Nothing = ???

  def execute(): Any = ???

}
