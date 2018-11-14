package hubmodel.supply.graph

import hubmodel.mgmt.ControlDevices
import hubmodel.mgmt.flowsep.FlowSeparator
import hubmodel.ped.{PedestrianNOMAD, PedestrianSim, PedestrianTrait}
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
  * @param baseVertices       set of vertices
  * @param standardEdges  connections between each vertex. THIS SHOULD BE MYEDGES PROBABLY ?
  * @param levelChanges   links where pedestrians can change levels
  * @param flowGates      links on which there are flow gates
  * @param binaryGates    collection of binary gates (one should probably use flow gates with binary control law)
  * @param movingWalkways collection of moving walkways (not yet implemented)
  * @param flowSeparators collection of flow separators
  */
class RouteGraph[T <: PedestrianNOMAD](private val baseVertices: Iterable[Rectangle],
                                     private val standardEdges: Iterable[MyEdge],
                                     private val levelChanges: Iterable[MyEdgeLevelChange],
                                     flowGates: Iterable[FlowGate],
                                     binaryGates: Iterable[BinaryGate],
                                     movingWalkways: Iterable[MovingWalkway],
                                     flowSeparators: Iterable[FlowSeparator]) extends RouteGraphParent[T](levelChanges, flowGates, binaryGates, movingWalkways, flowSeparators) {

  // Function to enqueue pedestrians into queues at gates.
  /*def enqueueInWaitingZone(p: T): Unit = {
    super.enqueueInWaitingZoneGen[T](flowGates)(p)
  }*/

  // Map from vertex names to the vertices themselves
  override val vertexMap: Map[String, Rectangle] = baseVertices.map(v => v.name -> v).toMap ++
    flowSeparators.flatMap(fs => fs.associatedZonesStart.map(oz => oz.name -> oz)).toMap ++
    flowSeparators.flatMap(fs => fs.associatedZonesEnd.map(oz => oz.name -> oz)).toMap --
    flowSeparators.flatMap(fs => fs.overridenZones)

  // builds the container for the graph
  private val network: DefaultDirectedWeightedGraph[Rectangle, MyEdge] = new DefaultDirectedWeightedGraph[Rectangle, MyEdge](classOf[MyEdge])

  // adds the vertices to the graph
  vertexMap.values.foreach(v => network.addVertex(v))

  // builds the edge set from the various strategies which modify the base graph.
  override val edges: Set[MyEdge] = (standardEdges ++ flowGates ++ binaryGates ++ movingWalkways).toSet
    .filterNot(e => flowSeparators.flatMap(_.associatedConnectivity.map(_.startVertex.name)).toVector.contains(e.startVertex.name)
      || flowSeparators.flatMap(_.associatedConnectivity.map(_.startVertex.name)).toVector.contains(e.endVertex.name)
    ) ++ flowSeparators.flatMap(_.associatedConnectivity)

  // adds the edges and sets the weights
  edges.foreach(e => {
    network.addEdge(e.startVertex, e.endVertex, e)
    network.setEdgeWeight(e, e.length)
  })

  def vertices: Iterable[Rectangle] = this.vertexMap.values

  // object used to get the shortest path in the network
  private var shortestPathBuilder: DijkstraShortestPath[Rectangle, MyEdge] = new DijkstraShortestPath[Rectangle, MyEdge](network)

  /**
    * Updates the cost of each edge in the graph based on the cost of the edges stored in the "edges" variable.
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
    Try(shortestPathBuilder.getPath(o, d)) match {
      case Success(s) => {
        s.getVertexList.asScala.toList
      }
      case Failure(f) => throw f
    }
  }

  /**
    * Determines whether there is a change in floor level when changing links. This is important as the pedestrians must be
    * "teleported" to the start of the other edge.
    */
  val isFloorChange: (Rectangle, Rectangle) => Boolean = (a, b) => this.levelChanges.map(e => (e.startVertex.ID, e.endVertex.ID)).exists(_ == (a.ID, b.ID))

  /**
    * Changes the pedestrian's intermediat destination when the current intermediat destination is reached.
    *
    * @param p pedestrian for whom to change destination
    */
  def processIntermediateArrival(p: T): Unit = {
    if (p.route.isEmpty){
      p.route = this.getShortestPath(p.origin, p.finalDestination).tail
      p.nextZone = p.route.head
    }
    else if (this.isFloorChange(p.nextZone, p.route.head)) {
      p.previousZone = p.route.head
      p.currentPosition = p.route.head.uniformSamplePointInside
      p.nextZone = p.route.tail.head
      p.route = this.getShortestPath(p.nextZone, p.finalDestination).tail
    }
    else {
      p.previousZone = p.nextZone
      p.route = this.getShortestPath(p.nextZone, p.finalDestination).tail
      p.nextZone = p.route.head
    }
    p.setCurrentDestination(p.nextZone.uniformSamplePointInside)
  }


  /**
    * Clones the graph, this should be thread safe and make hard copies of the objects os they can be used for
    * running multiple simulations at once.
    *
    * @param devices The new set of devices to use to make the graph. This way multiple graphs do not share control devices
    * @return Copy of the graph.
    */
  override def clone(devices: ControlDevices): RouteGraph[T] = new RouteGraph[T](
    this.baseVertices, this.standardEdges, this.levelChanges, devices.flowGates, devices.binaryGates, devices.amws, devices.flowSeparators
  )

}
