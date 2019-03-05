package hubmodel

import hubmodel.demand.PublicTransportSchedule
import hubmodel.mgmt.ControlDevices
import hubmodel.supply.NodeParent
import hubmodel.supply.continuous.ContinuousSpace
import hubmodel.supply.graph.GraphContainer
import hubmodel.tools.Time
import hubmodel.tools.cells.Rectangle

class SimulationParametersClass(val start: Time,
                                val end: Time,
                                val mvmtUpdate: Time,
                                val routeUpdate: Time,
                                val evaluateFrequency: Time,
                                val rebuildTreeInterval: Option[Time],
                                val microSpace: ContinuousSpace,
                                val graph: GraphContainer,
                                val timeTable: PublicTransportSchedule,
                                val stop2Vertex: NodeParent => Iterable[Rectangle],
                                val controlDevices: ControlDevices,
                                val writeTrajectoryData: Boolean)