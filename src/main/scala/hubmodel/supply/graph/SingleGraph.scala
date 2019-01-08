package hubmodel.supply.graph

import hubmodel.mgmt.ControlDevices
import hubmodel.mgmt.flowsep.FlowSeparator
import hubmodel.ped.PedestrianNOMAD
import hubmodel.tools.cells.Rectangle
import org.jgrapht.alg.shortestpath.DijkstraShortestPath
import org.jgrapht.graph.DefaultDirectedWeightedGraph

import scala.util.{Failure, Success, Try}

class SingleGraph(private val baseVertices: Iterable[Rectangle],
                  private val standardEdges: Iterable[MyEdge],
                  private val levelChanges: Iterable[MyEdgeLevelChange],
                  fg: Iterable[FlowGate],
                  bg: Iterable[BinaryGate],
                  mw: Iterable[MovingWalkway],
                  fs: Iterable[FlowSeparator]) extends GraphContainer(fg, bg, mw, fs) {

  private val graph = new RouteGraph(baseVertices, standardEdges, levelChanges, this.flowGates, this.binaryGates, this.movingWalkways, this.flowSeparators)

  def edges: Set[MyEdge] = this.graph.edgeCollection

  def vertexMapNew: Map[String, Rectangle] = this.graph.vertexCollection

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
  def clone(devices: ControlDevices): T = new SingleGraph(
    this.baseVertices, this.standardEdges, this.levelChanges, devices.flowGates, devices.binaryGates, devices.amws, devices.flowSeparators
  )

  def clone2AlternateGraphs(devices: ControlDevices, popFraction: Double): T = new SingleGraph(
    this.baseVertices, this.standardEdges, this.levelChanges, devices.flowGates, devices.binaryGates, devices.amws, devices.flowSeparators
  )

  override def toString: String = { this.graph.toString}
}
