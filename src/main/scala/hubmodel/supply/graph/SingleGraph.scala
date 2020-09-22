package hubmodel.supply.graph

import hubmodel.control.ControlDevices
import hubmodel.control.amw.MovingWalkwayAbstract
import hubmodel.control.flowgate.{BinaryGate, FlowGate}
import hubmodel.control.flowsep.FlowSeparator
import hubmodel.ped.PedestrianNOMAD
import tools.Time
import tools.cells.Vertex

class SingleGraph(private val baseVertices: Iterable[Vertex],
                  private val standardEdges: Iterable[MyEdge],
                  private val levelChanges: Iterable[MyEdgeLevelChange],
                  private val destinationGroups: Iterable[(String, Vector[String])],
                  fg: Iterable[FlowGate],
                  bg: Iterable[BinaryGate],
                  mw: Iterable[MovingWalkwayAbstract],
                  fs: Iterable[FlowSeparator[_, _]]) extends GraphContainer(fg, bg, mw, fs) {

  private val graph = new RouteGraph(baseVertices, standardEdges, levelChanges, this.flowGates, this.binaryGates, this.movingWalkways, this.flowSeparators, destinationGroups = destinationGroups)


  def updateGraphCosts(): Unit = {
    this.graph.updateGraph()
  }

  def edges: Set[MyEdge] = this.graph.edgeCollection

  def vertexMapNew: Map[String, Vertex] = this.graph.vertexCollection

  val verticesToEdgesMap: Map[(Vertex,Vertex), MyEdge] = this.edges.map(e => ((e.startVertex, e.endVertex),e)).toMap

  /**
    * Changes the pedestrian's intermediat destination when the current intermediat destination is reached.
    *
    * @param p pedestrian for whom to change destination
    */
  def processIntermediateArrival(t: Time, p: PedestrianNOMAD): Unit = {this.graph.processIntermediateArrival(t, p)}

  def processRouteOutOfZones(t: Time, p: PedestrianNOMAD): Unit = {this.graph.updateRouteOutsideZones(t, p) }

  def changeAMWStatus(ped: PedestrianNOMAD): Unit = { this.graph.changeAMWStatus(ped) }

  def computeODsWithAMWs: Map[(String, String), Vector[String]] = {
    this.graph.computeODs
  }

  type T = SingleGraph

  /**
    * Clones the graph, this should be thread safe and make hard copies of the objects os they can be used for
    * running multiple simulations at once.
    *
    * @param devices The new set of devices to use to make the graph. This way multiple graphs do not share control devices
    * @return Copy of the graph.
    */
  def deepCopy(devices: ControlDevices): T = new SingleGraph(
    this.baseVertices.map(_.deepCopy), this.standardEdges.map(_.deepCopy), this.levelChanges.map(_.deepCopy), this.destinationGroups, devices.flowGates, devices.binaryGates, devices.amws, devices.flowSeparators
  )

  def deepCopy2AlternateGraphs(devices: ControlDevices, popFraction: Double): T = new SingleGraph(
    this.baseVertices.map(_.deepCopy), this.standardEdges.map(_.deepCopy), this.levelChanges.map(_.deepCopy), this.destinationGroups, devices.flowGates, devices.binaryGates, devices.amws, devices.flowSeparators
  )

  override def toString: String = {
    this.graph.toString
  }
}
