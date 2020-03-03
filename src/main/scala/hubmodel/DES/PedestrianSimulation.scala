package hubmodel.DES

import hubmodel.control.ControlDevices
import hubmodel.demand.PublicTransportSchedule
import hubmodel.supply.continuous.ContinuousSpace
import hubmodel.supply.graph.{GraphContainer, Stop2Vertex}
import tools.Time

class PedestrianSimulation(st: Time,
                                   et: Time,
                                   sf_dt: Time,
                                   route_dt: Time,
                                   evaluate_dt: Time,
                                   rebuildTreeInterval: Option[Time],
                                   spaceMicro: ContinuousSpace,
                                   graph: GraphContainer,
                                   timeTable: Option[PublicTransportSchedule],
                                   stop2Vertex: Stop2Vertex,
                                   controlDevices: ControlDevices,
                                   logFullPedestrianHistory: Boolean = false) extends NOMADGraphSimulator(st,
                                                                                                  et,
                                                                                                  sf_dt,
                                                                                                  route_dt,
                                                                                                  evaluate_dt,
                                                                                                  rebuildTreeInterval,
                                                                                                  spaceMicro,
                                                                                                  graph,
                                                                                                  timeTable,
                                                                                                  stop2Vertex,
                                                                                                  controlDevices,
                                                                                                  logFullPedestrianHistory) with IsMainSimulation {

  override val simulationType: String = "main simulation"


  class StateEvaluation extends super.StateEval(this) with super.StateEvaluationActionDES

  insertStateEvaluationStart(new this.StateEvaluation)

}
