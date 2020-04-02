package hubmodel.DES

import hubmodel.control.ControlDevices
import hubmodel.demand.PublicTransportSchedule
import hubmodel.supply.continuous.ContinuousSpace
import hubmodel.supply.graph.{GraphContainer, Stop2Vertex}
import tools.Time

class PedestrianSimulation(params: SimulationInputParameters) extends NOMADGraphSimulator(params) with IsMainSimulation {

  val isPrediction: Boolean = false

  override val simulationType: String = "main simulation"

  class StateEvaluation extends super.StateEval(this) with super.StateEvaluationActionDES

  insertStateEvaluationStart(new this.StateEvaluation)

}
