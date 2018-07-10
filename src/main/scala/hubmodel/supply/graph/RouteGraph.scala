package hubmodel.supply.graph

import hubmodel.mgmt.flowsep.FlowSeparator
import hubmodel.ped.PedestrianSim
import hubmodel.tools.cells.Rectangle
import org.jgrapht.alg.shortestpath.DijkstraShortestPath
import org.jgrapht.graph.DefaultDirectedWeightedGraph

import scala.collection.JavaConverters._
import scala.util.{Failure, Success, Try}

/** Graph used for the computation of the route choice.
  *
  * Reference for the definition of the equality between two edges and vertices:
  * https://github.com/jgrapht/jgrapht/wiki/EqualsAndHashCode#equalshashcode-for-vertexedge-objects
  *
  * @param vertices      set of vertices
  * @param standardEdges connections between each vertex. THIS SHOULD BE MYEDGES PROBABLY ?
  * @param flowGates     links on which there are flow gates
  */
class RouteGraph(private val vertices: Vector[Rectangle],
                 private val standardEdges: Iterable[MyEdge],
                 flowGates: Iterable[FlowGate],
                 binaryGates: Iterable[BinaryGate],
                 movingWalkways: Iterable[MovingWalkway],
                 flowSeparators: Iterable[FlowSeparator]) extends WithGates {

  def enqueueInWaitingZone(p: PedestrianSim): Unit = {
    super.enqueueInWaitingZone(flowGates)(p)
  }

  // Map from vertex names to the vertices themselves
  val vertexMap: Map[String, Rectangle] = vertices.map(v => v.name -> v).toMap ++
    flowSeparators.flatMap(fs => fs.associatedZonesStart.map(oz => oz.name -> oz)).toMap ++
    flowSeparators.flatMap(fs => fs.associatedZonesEnd.map(oz => oz.name -> oz)).toMap --
    flowSeparators.flatMap(fs => fs.overridenZones)

  // building the graph
  private val network: DefaultDirectedWeightedGraph[Rectangle, MyEdge] = new DefaultDirectedWeightedGraph[Rectangle, MyEdge](classOf[MyEdge])
  vertexMap.values.foreach(v => network.addVertex(v))
  val edges: Set[MyEdge] = (standardEdges ++ flowGates ++ binaryGates ++ movingWalkways).toSet.
    filterNot(e => flowSeparators.flatMap(_.overridenZones).toVector.contains(e.startVertex.name) || flowSeparators.flatMap(_.overridenZones).toVector.contains(e.endVertex.name)) ++
    flowSeparators.flatMap(_.associatedConnectivity)

  edges.foreach(e => {
    network.addEdge(e.startVertex, e.endVertex, e)
    network.setEdgeWeight(e, e.length)
  })

  // object used to get the shortest path in the network
  private var shortestPathBuilder: DijkstraShortestPath[Rectangle, MyEdge] = new DijkstraShortestPath[Rectangle, MyEdge](network)

  /** Updates the cost of each edge in the graph based on the cost of the edges stored in the "edges" variable.
    * This method updates the cost of the edges before actually updating the graph object itslef.
    */
  def updateGraph(): Unit = {
    edges.foreach(_.updateCost(1.0))
    edges.foreach(e => network.setEdgeWeight(e, e.cost))
    this.shortestPathBuilder = new DijkstraShortestPath[Rectangle, MyEdge](network)
  }

  /** Uses to shortestPathBuilder to compute the shortest path between two vertices.
    *
    * @param o origin node
    * @param d destination node
    * @return the list if vertices representing the path
    */
  def getShortestPath(o: Rectangle, d: Rectangle): List[Rectangle] = {
    Try(shortestPathBuilder.getPath(o, d)/*.getVertexList.asScala.toList*/) match {
      case Success(s) => {
        s.getVertexList.asScala.toList
      }
      case Failure(f) => throw f
    }
  }

}
