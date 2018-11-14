package hubmodel.supply.graph

import hubmodel.mgmt.ControlDevices
import hubmodel.mgmt.flowsep.FlowSeparator
import hubmodel.ped._
import hubmodel.tools.cells.Rectangle

abstract class RouteGraphParent[+T <: PedestrianNOMAD] (private val levelChanges: Iterable[MyEdgeLevelChange],
                                                        private val flowGates: Iterable[FlowGate],
                                                        private val binaryGates: Iterable[BinaryGate],
                                                        private val movingWalkways: Iterable[MovingWalkway],
                                                        private val flowSeparators: Iterable[FlowSeparator]) {

  def vertexMap: Map[String, Rectangle]

  def edges: Set[MyEdge]

  //protected def getShortestPath(o: Rectangle, d: Rectangle, ID: Option[String] = None): List[Rectangle]

  def clone(devices: ControlDevices): RouteGraphParent[T]

}
