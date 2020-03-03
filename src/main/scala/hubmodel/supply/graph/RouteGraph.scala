package hubmodel.supply.graph

import hubmodel.control.ControlDevices
import hubmodel.control.amw.{IN, MovingWalkway, OUT}
import hubmodel.control.flowgate.{BinaryGate, FlowGate}
import hubmodel.control.flowsep.FlowSeparator
import hubmodel.ped.PedestrianNOMAD
import myscala.math.vector.ZeroVector2D
import org.jgrapht.alg.shortestpath.{DijkstraShortestPath, KShortestPaths}
import org.jgrapht.graph.DefaultDirectedWeightedGraph
import tools.cells.Vertex

import scala.collection.JavaConverters._
import scala.util.{Failure, Success, Try}

/** Graph used for the computation of the route choice.
  *
  * Reference for the definition of the equality between two edges and vertices:
  * https://github.com/jgrapht/jgrapht/wiki/EqualsAndHashCode#equalshashcode-for-vertexedge-objects
  *
  * @param baseVertices   set of vertices
  * @param standardEdges  connections between each vertex. THIS SHOULD BE MYEDGES PROBABLY ?
  * @param levelChanges   links where pedestrians can change levels
  * @param flowGates      links on which there are flow gates
  * @param binaryGates    collection of binary gates (one should probably use flow gates with binary control law)
  * @param movingWalkways collection of moving walkways (not yet implemented)
  * @param flowSeparators collection of flow separators
  */
class RouteGraph(protected val baseVertices: Iterable[Vertex],
                 protected val standardEdges: Iterable[MyEdge],
                 val levelChanges: Iterable[MyEdgeLevelChange],
                 protected val flowGates: Iterable[FlowGate],
                 protected val binaryGates: Iterable[BinaryGate],
                 protected val movingWalkways: Iterable[MovingWalkway],
                 protected val flowSeparators: Iterable[FlowSeparator[_, _]],
                 val edges2Add: Set[MyEdge] = Set(),
                 val edges2Remove: Set[MyEdge] = Set(),
                 val destinationGroups: Iterable[(String, Vector[String])]) {

  private val movingWalkwayVertices: Map[Vertex, (hubmodel.control.amw.Direction,MovingWalkway)] = movingWalkways.flatMap(amw => Vector(amw.startVertex -> (IN, amw), amw.endVertex -> (OUT, amw))).toMap

  // Collection of all vertices in the network. This map can be used to get a vertex based on it's name.
  val vertexCollection: Map[String, Vertex] = this.baseVertices.map(v => v.name -> v).toMap ++
    flowSeparators.flatMap(fs => fs.associatedZonesStart.map(oz => oz.name -> oz) ++ fs.associatedZonesEnd.map(oz => oz.name -> oz)) --
    flowSeparators.flatMap(fs => fs.overridenZones) ++
  movingWalkways.flatMap(amw => Map(amw.startVertex.name -> amw.startVertex, amw.endVertex.name -> amw.endVertex)) ++
  movingWalkways.flatMap(amw => amw.associatedZonesStart.map(oz => oz.name -> oz) ++ amw.associatedZonesEnd.map(oz => oz.name -> oz)) --
  movingWalkways.flatMap(amw => amw.droppedVertices)

  // Inverted destination groups which is used to check if alternative equivalent destination are available
  private val destination2EquivalentDestinations: Map[String, Vector[String]] = destinationGroups.flatMap(kv => kv._2.map(r => r -> kv._2)).toMap

  private def destination2EquivalentDestinations(zone: Vertex): Vector[Vertex] = destination2EquivalentDestinations.getOrElse(zone.name, Vector(zone.name)).map(zID => this.vertexCollection(zID))


  //def vertexMapNew: Map[String, Rectangle] = this.vertexCollection

  // builds the container for the graph
  private val network: DefaultDirectedWeightedGraph[Vertex, MyEdge] = new DefaultDirectedWeightedGraph[Vertex, MyEdge](classOf[MyEdge])

  // adds the vertices to the graph
  //println(vertexMapNew)
  this.vertexCollection.values.foreach(v => network.addVertex(v))

  // builds the edge set from the various strategies which modify the base graph.
  /*val edgeCollection: Set[MyEdge] = (((standardEdges ++ flowGates ++ binaryGates ++ movingWalkways ++ levelChanges).toSet
    .filterNot(e =>
      flowSeparators.flatMap(_.associatedConnectivity.map(_.startVertex.name)).toVector.contains(e.startVertex.name)
      || flowSeparators.flatMap(_.associatedConnectivity.map(_.startVertex.name)).toVector.contains(e.endVertex.name)
    ) ++ flowSeparators.flatMap(_.associatedConnectivity)) ++ edges2Add) -- edges2Remove*/


  val edgeCollection: Set[MyEdge] = (((standardEdges ++ flowGates ++ binaryGates ++ movingWalkways ++ levelChanges).toSet
    .filterNot(e =>
      flowSeparators.flatMap(fs => fs.overridenZones).toVector.contains(e.startVertex.name)
        || flowSeparators.flatMap(fs => fs.overridenZones).toVector.contains(e.endVertex.name)
      || movingWalkways.flatMap(amw => amw.droppedVertices).toVector.contains(e.startVertex.name)
        || movingWalkways.flatMap(amw => amw.droppedVertices).toVector.contains(e.endVertex.name)
    ) ++ flowSeparators.flatMap(_.associatedConnectivity) ++ movingWalkways.flatMap(_.associatedConnectivity)) ++ edges2Add) -- edges2Remove

  //def edges: Set[MyEdge] = edgeCollection

  // adds the edges and sets the weights
  this.edgeCollection.foreach(e => {
    network.addEdge(e.startVertex, e.endVertex, e)
    e match {
      case lv: MyEdgeLevelChange => { network.setEdgeWeight(lv, 0.0) }
      case a: MovingWalkway => {network.setEdgeWeight(a, a.length / (1.34 + a.speed))}
      case _ => { network.setEdgeWeight(e, e.length / 1.34) }
    }
  })

  //def vertices: Iterable[Rectangle] = this.vertexMapNew.values

  // object used to get the shortest path in the network
  private var shortestPathBuilder: DijkstraShortestPath[Vertex, MyEdge] = new DijkstraShortestPath[Vertex, MyEdge](network)

  // Construction for getting k shortest paths between and origin and a destination in the graph
  private var multipleShortestPathsBuilder: KShortestPaths[Vertex, MyEdge] = new KShortestPaths[Vertex, MyEdge](network, 5)


  /**
    * Updates the cost of each edge in the graph based on the cost of the edges stored in the "edges" variable.
    * This method updates the cost of the edges before actually updating the graph object itslef.
    */
  private def updateGraph(): Unit = {
    this.edgeCollection.foreach(_.updateCost(1.0))
    this.edgeCollection.foreach(e => network.setEdgeWeight(e, e.cost))
    this.shortestPathBuilder = new DijkstraShortestPath[Vertex, MyEdge](network)
    this.multipleShortestPathsBuilder = new KShortestPaths[Vertex, MyEdge](network, 5)
  }

  /** Uses to shortestPathBuilder to compute the shortest path between two vertices.
    *
    * @param o origin node
    * @param d destination node
    * @return the list if vertices representing the path
    */
  private def getShortestPath(o: Vertex, d: Vertex): (Double, List[Vertex]) = {

    // Chooses the route between the five shortest ones and selects it using the logit model with equal weights.
    /*val paths: Seq[GraphPath[Rectangle, MyEdge]] = this.multipleShortestPathsBuilder.getPaths(o, d).asScala.toVector
    val weightsSum = paths.map(v => math.exp(-math.log(v.getWeight))).sum
    val probabilities: Seq[Double] = paths.map(p => math.exp(-math.log(p.getWeight))/weightsSum)
    val path = paths(probabilities.scanLeft(0.0)(_ + _).tail.indexWhere(p => ThreadLocalRandom.current().nextDouble() < p)).getVertexList*/


    Try(shortestPathBuilder.getPath(o, d)) match {
      case Success(s) if (s.getVertexList.size() > 0) => {
        (s.getWeight, s.getVertexList.asScala.toList)
      }
      case Failure(f) => {
        throw f
      }
      case _ => {
        throw new IllegalAccessError("No route from " + o + " to " + d)
      }
    }
  }

  /**
    * Determines whether there is a change in floor level when changing links. This is important as the pedestrians must be
    * "teleported" to the start of the other edge.
    */
  private def isFloorChange(a: Vertex, b: Vertex): Boolean = {
    this.levelChanges.map(e => (e.startVertex.ID, e.endVertex.ID)).exists(_ == (a.ID, b.ID))
  }


  /**
    * Changes the pedestrian's intermediat destination when the current intermediat destination is reached.
    *
    * @param p pedestrian for whom to change destination
    */
  def processIntermediateArrival(p: PedestrianNOMAD): Unit = {
    //println(p.route)
    if (p.route.isEmpty) {
      p.route = destination2EquivalentDestinations(p.finalDestination).filter(_ != p.origin).map(d => this.getShortestPath(p.origin, d)).minBy(_._1)._2.tail
      p.finalDestination = p.route.last
      p.nextZone = p.route.head
      p.route = p.route.tail
    }
    else if (this.isFloorChange(p.nextZone, p.route.head)) {
      p.previousZone = p.route.head
      p.currentPosition = p.route.head.uniformSamplePointInside
      p.previousPosition = p.currentPosition
      p.nextZone = p.route.tail.head
      p.route = destination2EquivalentDestinations(p.finalDestination).filter(_ != p.origin).map(d => this.getShortestPath(p.nextZone, d)).minBy(_._1)._2.tail
      p.finalDestination = p.route.last
    }
    else {
      p.previousZone = p.nextZone
      p.route = destination2EquivalentDestinations(p.finalDestination).filter(_ != p.origin).map(d => this.getShortestPath(p.previousZone, d)).minBy(_._1)._2.tail
      p.finalDestination = p.route.last
      p.nextZone = p.route.head
      p.route = p.route.tail
    }
    p.setCurrentDestination(p.nextZone.uniformSamplePointInside)

    // update the base moving speed of pedestrian p if he is entering a or exiting a moving walkway
    movingWalkwayVertices.get(p.previousZone).collect {
      case amw if amw._1 == IN => p.baseVelocity = amw._2.movingSpeed
      case amw if amw._1 == OUT => p.baseVelocity = new ZeroVector2D
    }
  }


  /**
    * Clones the graph, this should be thread safe and make hard copies of the objects os they can be used for
    * running multiple simulations at once.
    *
    * @param devices The new set of devices to use to make the graph. This way multiple graphs do not share control devices
    * @return Copy of the graph.
    */
  def clone(devices: ControlDevices): RouteGraph = new RouteGraph(
    this.baseVertices, this.standardEdges, this.levelChanges, devices.flowGates, devices.binaryGates, devices.amws, devices.flowSeparators, destinationGroups = this.destinationGroups
  )

}
