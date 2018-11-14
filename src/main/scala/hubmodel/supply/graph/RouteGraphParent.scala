package hubmodel.supply.graph

import hubmodel.mgmt.ControlDevices
import hubmodel.ped._
import hubmodel.tools.cells.Rectangle

abstract class RouteGraphParent[+T <: PedestrianNOMAD] extends WithGates {

  //type V <: PedestrianNOMAD

  //def processIntermediateArrival(ped: V): Unit

  def vertexMap: Map[String, Rectangle]

  def edges: Set[MyEdge]

  def getShortestPath(o: Rectangle, d: Rectangle, id: Option[String] = None): List[Rectangle]

  def clone(devices: ControlDevices): RouteGraphParent[T]
}
