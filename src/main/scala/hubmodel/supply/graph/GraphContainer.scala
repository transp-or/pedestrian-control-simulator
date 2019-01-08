package hubmodel.supply.graph

import hubmodel.mgmt.ControlDevices
import hubmodel.mgmt.flowsep.FlowSeparator
import hubmodel.ped.PedestrianNOMAD
import hubmodel.tools.cells.Rectangle

abstract class GraphContainer(protected val flowGates: Iterable[FlowGate],
                              protected val binaryGates: Iterable[BinaryGate],
                              protected val movingWalkways: Iterable[MovingWalkway],
                              protected val flowSeparators: Iterable[FlowSeparator]) {

  def vertexMapNew: Map[String, Rectangle]

  def edges: Set[MyEdge]

  def processIntermediateArrival(ped: PedestrianNOMAD): Unit

  type T <: GraphContainer
  def clone(devices: ControlDevices): T
  def clone2AlternateGraphs(devices: ControlDevices, populationFraction: Double): T

}
