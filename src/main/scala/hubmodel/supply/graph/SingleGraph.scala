package hubmodel.supply.graph

import hubmodel.control.ControlDevices
import hubmodel.control.amw.MovingWalkway
import hubmodel.control.flowgate.{BinaryGate, FlowGate}
import hubmodel.control.flowsep.FlowSeparator
import hubmodel.ped.PedestrianNOMAD
import tools.cells.Vertex

class SingleGraph(private val baseVertices: Iterable[Vertex],
                  private val standardEdges: Iterable[MyEdge],
                  private val levelChanges: Iterable[MyEdgeLevelChange],
                  private val destinationGroups: Iterable[(String, Vector[String])],
                  fg: Iterable[FlowGate],
                  bg: Iterable[BinaryGate],
                  mw: Iterable[MovingWalkway],
                  fs: Iterable[FlowSeparator[_, _]]) extends GraphContainer(fg, bg, mw, fs) {

  private val graph = new RouteGraph(baseVertices, standardEdges, levelChanges, this.flowGates, this.binaryGates, this.movingWalkways, this.flowSeparators, destinationGroups = destinationGroups)

  def edges: Set[MyEdge] = this.graph.edgeCollection

  def vertexMapNew: Map[String, Vertex] = this.graph.vertexCollection

  /**
    * Changes the pedestrian's intermediat destination when the current intermediat destination is reached.
    *
    * @param p pedestrian for whom to change destination
    */
  def processIntermediateArrival(p: PedestrianNOMAD): Unit = this.graph.processIntermediateArrival(p)


  type T = SingleGraph

  /**
    * Clones the graph, this should be thread safe and make hard copies of the objects os they can be used for
    * running multiple simulations at once.
    *
    * @param devices The new set of devices to use to make the graph. This way multiple graphs do not share control devices
    * @return Copy of the graph.
    */
  def deepCopy(devices: ControlDevices): T = new SingleGraph(
    this.baseVertices, this.standardEdges, this.levelChanges, this.destinationGroups, devices.flowGates, devices.binaryGates, devices.amws, devices.flowSeparators
  )

  def deepCopy2AlternateGraphs(devices: ControlDevices, popFraction: Double): T = new SingleGraph(
    this.baseVertices, this.standardEdges, this.levelChanges, this.destinationGroups, devices.flowGates, devices.binaryGates, devices.amws, devices.flowSeparators
  )

  override def toString: String = {
    this.graph.toString
  }
}
