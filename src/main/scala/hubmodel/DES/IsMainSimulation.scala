package hubmodel.DES

import hubmodel.P
import hubmodel.control.{EvaluateState, StateEvaluationAction, UpdateGates}
import hubmodel.ped.PedestrianNOMAD
import hubmodel.prediction.{DemandEstimateFromGroundTruth, RunPrediction, StatePrediction}
import tools.Time

trait IsMainSimulation {

  /** container for keeping the prediction results */
  private var _prediction: Option[StatePrediction] = None

  def prediction: Option[StatePrediction] = this._prediction

  def updatePrediction(statePrediction: StatePrediction): Unit = {
    this._prediction = Some(statePrediction)
  }

  class StateEval(sim: PedestrianSimulation) extends EvaluateState(sim) with Action {

    override def execute(): Unit = {

      sim.eventLogger.trace("sim-time=" + sim.currentTime + ": state evaluation")

      this.computeDensityAtCurrentTime()

      if (sim.useFlowGates || sim.useBinaryGates) {
        sim.insertEventWithZeroDelay(new UpdateGates(sim))
      }

      if (sim.useFlowSep && !sim.controlDevices.fixedFlowSeparators) {
        processIncomingFlowsForFS()
      }

      if (! this.sim.isPrediction) {
        sim.insertEventWithZeroDelay(new RunPrediction(this.sim))
      }

      val flows: Option[DemandEstimateFromGroundTruth] = sim.prediction.map(pred => {
        new DemandEstimateFromGroundTruth(
          this.sim.prediction.get.getPredictedStateData.population,
          this.sim.prediction.get.getPredictedStateData.controlDevices.amws.toVector,
          this.sim.currentTime.value.to(this.sim.currentTime.value + 3*this.sim.evaluate_dt.value).by(30).toVector.map(v => Time(v.toDouble))
        )
      })

      this.sim.insertEventWithDelay(sim.evaluate_dt)(new StateEval(sim))
    }



    type A = StateEval

    type B = PedestrianSimulation

    override def deepCopy(simulator: B): Option[A] = {

      Some(new StateEval(simulator))

    }

  }
}
