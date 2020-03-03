package hubmodel

import hubmodel.control.ControlDevices
import hubmodel.demand.PublicTransportSchedule
import hubmodel.supply.continuous.ContinuousSpace
import hubmodel.supply.graph.{GraphContainer, Stop2Vertex}
import tools.Time

class SimulationParametersClass(val start: Time,
                                val end: Time,
                                val mvmtUpdate: Time,
                                val routeUpdate: Time,
                                val evaluateFrequency: Time,
                                val rebuildTreeInterval: Option[Time],
                                val microSpace: ContinuousSpace,
                                val graph: GraphContainer,
                                val timeTable: Option[PublicTransportSchedule],
                                val stop2Vertex: Stop2Vertex,
                                val controlDevices: ControlDevices,
                                val writeTrajectoryData: Boolean)