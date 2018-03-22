package hubmodel.supply.graph

import hubmodel.Vertex
import hubmodel.mgmt.FlowSeparator
import hubmodel.ped.PedestrianSim
import org.jgrapht.alg.shortestpath.DijkstraShortestPath
import org.jgrapht.graph.DefaultDirectedWeightedGraph

import scala.collection.JavaConverters._

/** Graph used for the computation of the route choice.
  *
  * Reference for the definition of the equality between two edges and vertices:
  * https://github.com/jgrapht/jgrapht/wiki/EqualsAndHashCode#equalshashcode-for-vertexedge-objects
  *
  * @param vertices      set of vertices
  * @param standardEdges connections between each vertex. THIS SHOULD BE MYEDGES PROBABLY ?
  * @param flowGates     links on which there are flow gates
  */
class RouteGraph(private val vertices: Vector[Vertex],
                 val standardEdges: Iterable[MyEdge],
                 flowGates: Iterable[FlowGate],
                 binaryGates: Iterable[BinaryGate],
                 movingWalkways: Iterable[MovingWalkway],
                 flowSeparators: Iterable[FlowSeparator]) extends WithGates {

  def enqueueInWaitingZone(p: PedestrianSim): Unit = {
    super.enqueueInWaitingZone(flowGates)(p)
  }

  // Map from vertex names to the vertices themselves
  val vertexMap: Map[String, Vertex] = vertices.map(v => v.name -> v).toMap

  // building the graph
  private val network: DefaultDirectedWeightedGraph[Vertex, MyEdge] = new DefaultDirectedWeightedGraph[Vertex, MyEdge](classOf[MyEdge])
  vertices.foreach(v => network.addVertex(v))
  val allEdges: Iterable[MyEdge] = standardEdges ++ flowGates ++ binaryGates ++ movingWalkways

  allEdges.foreach(e => {
    network.addEdge(e.startVertex, e.endVertex, e)
    network.setEdgeWeight(e, e.length)
  })

  // object used to get the shortest path in the network
  private var shortestPathBuilder: DijkstraShortestPath[Vertex, MyEdge] = new DijkstraShortestPath[Vertex, MyEdge](network)

  /** Updates the cost of each edge in the graph based on the cost of the edges stored in the "edges" variable.
    * This method updates the cost of the edges before actually updating the graph object itslef.
    */
  def updateGraph(): Unit = {
    allEdges.foreach(_.updateCost(1.0))
    allEdges.foreach(e => network.setEdgeWeight(e, e.cost))
    this.shortestPathBuilder = new DijkstraShortestPath[Vertex, MyEdge](network)
  }

  /** Uses to shortestPathBuilder to compute the shortest path between two vertices.
    *
    * @param o origin node
    * @param d destination node
    * @return the list if vertices representing the path
    */
  def getShortestPath(o: Vertex, d: Vertex): List[Vertex] = {
    shortestPathBuilder.getPath(o, d).getVertexList.asScala.toList
  }

}
