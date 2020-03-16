package hubmodel.DES

import hubmodel.control.{EvaluateState, UpdateGates}

trait IsPrediction {

  class StateEval(sim: PedestrianPrediction) extends EvaluateState(sim) with Action {
    override def execute(): Unit = {

      sim.eventLogger.trace("sim-time=" + sim.currentTime + ": state evaluation")

      this.computeDensityAtCurrentTime()

      if (sim.useFlowGates || sim.useBinaryGates) {
        sim.insertEventWithZeroDelay(new UpdateGates(sim))
      }

      if (sim.useFlowSep && !sim.controlDevices.fixedFlowSeparators) {
        processIncomingFlowsForFS()
      }

      this.sim.insertEventWithDelay(sim.stateEvaluationInterval)(new StateEval(sim))
    }

    type A = StateEval

    override def deepCopy(simulator: PedestrianPrediction): Option[A] = None
  }

}
