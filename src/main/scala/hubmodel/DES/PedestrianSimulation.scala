package hubmodel.DES

import hubmodel.control.ControlDevices
import hubmodel.demand.PublicTransportSchedule
import hubmodel.supply.continuous.ContinuousSpace
import hubmodel.supply.graph.{GraphContainer, Stop2Vertex}
import optimization.ALNS.ALNSParameters
import tools.Time

class PedestrianSimulation(params: SimulationInputParameters) extends NOMADGraphSimulator(params) with IsMainSimulation {

  val isPrediction: Boolean = false

  override val simulationType: String = "main simulation"

  class StateEvaluation extends super.StateEval(this) with super.StateEvaluationActionDES

  class LogStateSimulation extends super.LogState(this) with super.LogStateDES

  this.insertEventWithZeroDelay(new LogStateSimulation)

  insertStateEvaluationStart(new this.StateEvaluation)

  if (this.controlDevices.amws.nonEmpty && this.controlDevices.amwsMode._1 == "predictive") {
    this.insertEventWithZeroDelay(new RollingHorizonOptimization(this))
  }

}
