package hubmodel.supply.graph

import hubmodel.control.ControlDevices
import hubmodel.control.amw.MovingWalkwayAbstract
import hubmodel.control.flowgate.{BinaryGate, FlowGate}
import hubmodel.control.flowsep.FlowSeparator
import hubmodel.ped.PedestrianNOMAD
import tools.Time
import tools.cells.Vertex

abstract class GraphContainer(protected val flowGates: Iterable[FlowGate],
                              protected val binaryGates: Iterable[BinaryGate],
                              protected val movingWalkways: Iterable[MovingWalkwayAbstract],
                              protected val flowSeparators: Iterable[FlowSeparator[_, _]]) {

  def vertexMapNew: Map[String, Vertex]

  def edges: Set[MyEdge]

  def processIntermediateArrival(t: Time, ped: PedestrianNOMAD): Unit

  def processRouteOutOfZones(t: Time, ped: PedestrianNOMAD): Unit

  def changeAMWStatus(ped: PedestrianNOMAD): Unit

  def computeODsWithAMWs: Map[(String, String), Vector[String]]

  def updateGraphCosts(): Unit

  type T <: GraphContainer

  /** Creates a deep copy of the graph to use elsewhere.
    *
    * TODO: This needs to be improved when the edge weights are updated.
    *
    * @param devices new set of devices to use.
    * @return deep copy of graph
    */
  def deepCopy(devices: ControlDevices): T

  /** Creates a copy of the graph with the fraction of people using this alternative.
    *
    * @param devices            new set of devices to use
    * @param populationFraction fraction to use the alternate graph
    * @return
    */
  def deepCopy2AlternateGraphs(devices: ControlDevices, populationFraction: Double): T

}
