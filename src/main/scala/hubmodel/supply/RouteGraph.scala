package hubmodel.supply

import hubmodel.{PedestrianSim, VertexRectangle}
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
class RouteGraph(private val vertices: Vector[VertexRectangle],
                 val standardEdges: Iterable[MyEdge],
                 flowGates: Iterable[FlowGate],
                 binaryGates: Iterable[BinaryGate],
                 movingWalkways: Iterable[MovingWalkway]) extends WithGates {

  def enqueueInWaitingZone(p: PedestrianSim): Unit = {
    super.enqueueInWaitingZone(flowGates)(p)
  }

  // Map from vertex names to the vertices themselves
  val vertexMap: Map[String, VertexRectangle] = vertices.map(v => v.name -> v).toMap

  // building the graph
  private val network: DefaultDirectedWeightedGraph[VertexRectangle, MyEdge] = new DefaultDirectedWeightedGraph[VertexRectangle, MyEdge](classOf[MyEdge])
  vertices.foreach(v => network.addVertex(v))
  val allEdges: Iterable[MyEdge] = standardEdges ++ flowGates ++ binaryGates ++ movingWalkways

  allEdges.foreach(e => {
    network.addEdge(e.startVertex, e.endVertex, e)
    network.setEdgeWeight(e, e.length)
  })

  // object used to get the shortest path in the network
  private var shortestPathBuilder: DijkstraShortestPath[VertexRectangle, MyEdge] = new DijkstraShortestPath[VertexRectangle, MyEdge](network)

  /** Updates the cost of each edge in the graph based on the cost of the edges stored in the "edges" variable.
    * This method updates the cost of the edges before actually updating the graph object itslef.
    */
  def updateGraph(): Unit = {
    allEdges.foreach(_.updateCost(1.0))
    allEdges.foreach(e => network.setEdgeWeight(e, e.cost))
    this.shortestPathBuilder = new DijkstraShortestPath[VertexRectangle, MyEdge](network)
  }

  /** Uses to shortestPathBuilder to compute the shortest path between two vertices.
    *
    * @param o origin node
    * @param d destination node
    * @return the list if vertices representing the path
    */
  def getShortestPath(o: VertexRectangle, d: VertexRectangle): List[VertexRectangle] = {
    shortestPathBuilder.getPath(o, d).getVertexList.asScala.toList
  }

  /** Creates a [[hubmodel.Position]] inside the specific [[hubmodel.VertexRectangle]] corresponding to the name passed as argument.
    *
    * @param node name of the [[hubmodel.VertexRectangle]] to generate a point inside
    * @return [[hubmodel.Position]] uniformly sampled inside
    */
  /*def generateInZone(node: String): Position = {
    val rand = breeze.stats.distributions.Uniform(0, 1).sample(2)
    breeze.linalg.DenseVector(vertexMap(node).A(0) + rand(0) * (vertexMap(node).B(0) - vertexMap(node).A(0)), vertexMap(node).A(1) + rand(1) * (vertexMap(node).D(1) - vertexMap(node).A(1)))
  }*/
}
