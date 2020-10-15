package hubmodel.supply.graph

import java.util.concurrent.ThreadLocalRandom

import hubmodel.control.ControlDevices
import hubmodel.control.amw.{IN, MovingWalkwayAbstract, OUT}
import hubmodel.control.flowgate.{BinaryGate, FlowGate}
import hubmodel.control.flowsep.FlowSeparator
import hubmodel.ped.PedestrianNOMAD
import myscala.math.vector.ZeroVector2D
import org.jgrapht.alg.shortestpath.{DijkstraShortestPath, YenKShortestPath}
import org.jgrapht.graph.DefaultDirectedWeightedGraph
import tools.Time
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
                 protected val movingWalkways: Iterable[MovingWalkwayAbstract],
                 protected val flowSeparators: Iterable[FlowSeparator[_, _]],
                 val edges2Add: Set[MyEdge] = Set(),
                 val edges2Remove: Set[MyEdge] = Set(),
                 val destinationGroups: Iterable[(String, Vector[String])],
                 val beta: Double) {

  private val movingWalkwayVertices: Vector[Vertex] = movingWalkways.flatMap(w => Vector(w.firstVertex, w.secondVertex)).toVector

  // Collection of all vertices in the network. This map can be used to get a vertex based on it's name.
  val vertexCollection: Map[String, Vertex] = this.baseVertices.map(v => v.name -> v).toMap ++
    flowSeparators.flatMap(fs => fs.associatedZonesStart.map(oz => oz.name -> oz) ++ fs.associatedZonesEnd.map(oz => oz.name -> oz)) --
    flowSeparators.flatMap(fs => fs.overridenZones) ++
    movingWalkways.flatMap(amw => Map(amw.firstVertex.name -> amw.firstVertex, amw.secondVertex.name -> amw.secondVertex)) ++
    movingWalkways.flatMap(amw => amw.associatedZonesStart.map(oz => oz.name -> oz) ++ amw.associatedZonesEnd.map(oz => oz.name -> oz)) --
    movingWalkways.flatMap(amw => amw.droppedVertices)

  // Inverted destination groups which is used to check if alternative equivalent destination are available
  private val destination2EquivalentDestinations: Map[String, Vector[String]] = destinationGroups.flatMap(kv =>  kv._2.map(r => r -> kv._2)).toMap

  // builds the container for the graph
  private val network: DefaultDirectedWeightedGraph[Vertex, MyEdge] = new DefaultDirectedWeightedGraph[Vertex, MyEdge](classOf[MyEdge])



  private def destination2EquivalentDestinationsFunc(zone: Vertex): Vector[Vertex] = destination2EquivalentDestinations.getOrElse(zone.name, Vector(zone.name)).map(zID => this.vertexCollection(zID))

  this.vertexCollection.values.foreach(v => network.addVertex(v))
  //def vertexMapNew: Map[String, Rectangle] = this.vertexCollection


  // adds the vertices to the graph
  //println(vertexMapNew)

  // builds the edge set from the various strategies which modify the base graph.
  /*val edgeCollection: Set[MyEdge] = (((standardEdges ++ flowGates ++ binaryGates ++ movingWalkways ++ levelChanges).toSet
    .filterNot(e =>
      flowSeparators.flatMap(_.associatedConnectivity.map(_.startVertex.name)).toVector.contains(e.startVertex.name)
      || flowSeparators.flatMap(_.associatedConnectivity.map(_.startVertex.name)).toVector.contains(e.endVertex.name)
    ) ++ flowSeparators.flatMap(_.associatedConnectivity)) ++ edges2Add) -- edges2Remove*/

  //val edgeCollectionWithoutAMW: Set[MyEdge] = (standardEdges ++ flowGates ++ binaryGates ++ levelChanges).toSet

  val edgeCollection: Set[MyEdge] = (((standardEdges ++ flowGates ++ binaryGates ++ levelChanges).toSet ++ movingWalkways.flatMap(_.graphEdges)
    .filterNot(e =>
      flowSeparators.flatMap(fs => fs.overridenZones).toVector.contains(e.startVertex.name)
        || flowSeparators.flatMap(fs => fs.overridenZones).toVector.contains(e.endVertex.name)
        || movingWalkways.flatMap(amw => amw.droppedVertices).toVector.contains(e.startVertex.name)
        || movingWalkways.flatMap(amw => amw.droppedVertices).toVector.contains(e.endVertex.name)
    ) ++ flowSeparators.flatMap(_.associatedConnectivity) ++ movingWalkways.flatMap(_.associatedConnectivity)) ++ edges2Add) -- edges2Remove

  val edgeCollectionWithoutAMW: Set[MyEdge] = edgeCollection.filterNot(e => movingWalkwayVertices.contains(e.startVertex) && movingWalkwayVertices.contains(e.endVertex))

  //def edges: Set[MyEdge] = edgeCollection

  // adds the edges and sets the weights
  this.edgeCollection.foreach(e => {
    network.addEdge(e.startVertex, e.endVertex, e)
    e match {
      case lv: MyEdgeLevelChange => {
        network.setEdgeWeight(lv, 0.0)
      }
      case a: MovingWalkwayAbstract => {
        a.graphEdges.foreach(e => network.setEdgeWeight(e, e.cost))
      }
      case _ => {
        network.setEdgeWeight(e, e.length / 1.34)
      }
    }
  })

  //def vertices: Iterable[Rectangle] = this.vertexMapNew.values

  // object used to get the shortest path in the network
  private var shortestPathBuilder: DijkstraShortestPath[Vertex, MyEdge] = new DijkstraShortestPath[Vertex, MyEdge](network)
  private var KShortestPathBuilder: YenKShortestPath[Vertex, MyEdge] = new YenKShortestPath[Vertex, MyEdge](network)



  //computeODs

  /**
    * Updates the cost of each edge in the graph based on the cost of the edges stored in the "edges" variable.
    * This method updates the cost of the edges before actually updating the graph object itslef.
    */
  def updateGraph(): Unit = {
    //this.edgeCollection.foreach(e => e.updateCost(e.length))
    //this.edgeCollection.foreach(e => println(e.startVertex, e.endVertex, e.cost))
    this.edgeCollection.foreach(e => network.setEdgeWeight(e, e.cost))
    //println(this.network.edgeSet().asScala.toVector.map(v => (v.ID, v.costHistory)).sortBy(_._1).head)
    this.shortestPathBuilder = new DijkstraShortestPath[Vertex, MyEdge](network)
    this.KShortestPathBuilder = new YenKShortestPath[Vertex, MyEdge](network)
    //this.multipleShortestPathsBuilder = new KShortestPaths[Vertex, MyEdge](network, 5)
  }

  /** Uses to shortestPathBuilder to compute the shortest path between two vertices.
    *
    * @param o origin node
    * @param d destination node
    * @return the list if vertices representing the path
    */
  private def getShortestPath(o: Vertex, d: Vertex): (Double, List[Vertex]) = {

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

  private def getKShortestPath(o: Vertex, d: Vertex): Vector[(Double, List[Vertex])] = {

    Try(KShortestPathBuilder.getPaths(o, d, 3)) match {
      case Success(paths) if (paths.size() > 0) => {
        paths.asScala.toVector.map(s => (s.getWeight, s.getVertexList.asScala.toList))
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



  def updateRouteOutsideZones(t: Time, p: PedestrianNOMAD): Unit = {
    p.route = routeChoicePathSize(p.previousZone, p.finalDestination)._2.tail
    p.finalDestination = p.route.last
    p.nextZone = p.route.head
    p.route = p.route.tail
    p.setCurrentDestination(p.nextZone.uniformSamplePointInside)
  }

  /** Path size computation according to
    * "DISCRETE CHOICE MODELS WITH APPLICATIONS TO DEPARTURE TIME AND ROUTE CHOICEMoshe Ben-Akiva and Michel Bierlaire"
    * equation 3 page 22.
    *
    * @param routes
    * @return
    */
  def pathSize(routes: Vector[(List[Vertex], Double)]): Vector[Double] = {
    val shortestPath = routes.minBy(_._2)
    routes.map(r => {
      r._1.sliding(2).map(e => {
        val edgeCost: Double = this.edgeCollection.find(edge => edge.startVertex == e(0) && edge.endVertex == e(1)).get.cost
        val denom: Double = routes.filter(rr => rr._1.containsSlice(e)).map(rr => shortestPath._2 / rr._2).sum
        (edgeCost / r._2)/(denom)
      }).sum
    })
  }


  def routeChoicePathSize(origin: Vertex, destination: Vertex) = {
    val routes: Vector[(Double, List[Vertex])] = destination2EquivalentDestinationsFunc(destination).filter(_ != origin).flatMap(d => this.getKShortestPath(origin, d)).sortBy(_._1)
    val routesWithPathSizes = routes.zip(pathSize(routes.map(_.swap)))
    val denom = routesWithPathSizes.map(t => t._2 * math.exp(-beta * t._1._1)).sum
    val p: Double = ThreadLocalRandom.current().nextDouble()
    val selected: Int = routesWithPathSizes.map(t => t._2 * math.exp(-beta * t._1._1) / denom).scanLeft(0.0)(_ + _).zipWithIndex.takeWhile(_._1 < p).last._2
    routes(selected)
    //this.getShortestPath(origin, destination)
  }

  /**
    * Changes the pedestrian's intermediat destination when the current intermediat destination is reached.
    *
    * @param p pedestrian for whom to change destination
    */
  def processIntermediateArrival(t: Time, p: PedestrianNOMAD): Unit = {
    //println(p.route)
    if (p.route.isEmpty) {
      p.route = routeChoicePathSize(p.origin, p.finalDestination)._2.tail
      p.finalDestination = p.route.last
      p.nextZone = p.route.head
      p.route = p.route.tail
    }
    else if (this.isFloorChange(p.nextZone, p.route.head)) {
      p.previousZone = p.route.head
      p.setCurrentPosition(p.route.head.uniformSamplePointInside)
      p.previousPosition = p.currentPosition
      p.nextZone = p.route.tail.head
      val tmpRoute = routeChoicePathSize(p.nextZone, p.finalDestination)._2
      p.route = tmpRoute.tail
      p.finalDestination = tmpRoute.last
    }
    else {
      p.previousZone = p.nextZone
      p.route = routeChoicePathSize(p.previousZone, p.finalDestination)._2.tail
      p.finalDestination = p.route.last
      p.nextZone = p.route.head
      p.route = p.route.tail
    }
    p.setCurrentDestination(p.nextZone.uniformSamplePointInside)

    // update the cost of all edges EXCEPT for the AMW edges.
    if (p.accomplishedRoute.size >= 2) {
      this.edgeCollectionWithoutAMW
        .find(e => e.startVertex == p.accomplishedRoute.dropRight(1).last._2 && e.endVertex == p.accomplishedRoute.last._2)
        .foreach(e => {
          e.updateCost(t, (p.accomplishedRoute.last._1 - p.accomplishedRoute.dropRight(1).last._1).value.toDouble)
        })
    }

    // update the base moving speed of pedestrian p if he is entering or exiting a moving walkway
    changeAMWStatus(p)
  }

  def changeAMWStatus(ped: PedestrianNOMAD): Unit = {

    val nextZoneIsAMW = this.movingWalkways.find(w => (w.firstVertex == ped.previousZone && w.secondVertex == ped.nextZone) || (w.secondVertex == ped.previousZone && w.firstVertex == ped.nextZone))
    val leavingAMW = this.movingWalkways.find(w => (w.secondVertex == ped.previousZone && w.firstVertex != ped.nextZone) || (w.firstVertex == ped.previousZone && w.secondVertex != ped.nextZone))


    if (nextZoneIsAMW.isDefined && leavingAMW.isEmpty) {
      // entering amw
      ped.baseVelocity = nextZoneIsAMW.get.movingSpeed
      nextZoneIsAMW.get.addPedToAMW(ped.ID)
      ped.isInsideAMW = Some(nextZoneIsAMW.get.name)
    }
    else if (nextZoneIsAMW.isEmpty && leavingAMW.isDefined) {
      // leaving amw
      ped.baseVelocity = { t => new ZeroVector2D }
      leavingAMW.get.removePedFromAMW(ped.ID)
      ped.isInsideAMW = None

    }
    else if (nextZoneIsAMW.isDefined && leavingAMW.isDefined) {
      throw new Exception("Error with pedestrian leaving AMW. This case should not happen ! ")
    }
  }

  def computeODs: Map[(String, String), Vector[String]] = {
    // Construction for getting k shortest paths between and origin and a destination in the graph
    this.movingWalkways.foreach(w => w.graphEdges.foreach(e => e.updateCost(Time(-1), 0.0)))
    val multipleShortestPathsBuilder: YenKShortestPath[Vertex, MyEdge] = new YenKShortestPath[Vertex, MyEdge](network)

    val amwEdgesIDs: Map[String, String] = this.movingWalkways.flatMap(w => w.graphEdges.map(e => e.ID -> w.name)).toMap

    this.vertexCollection
      .filter(_._2.isOD)
      .keys.toVector
      .combinations(2)
      .flatMap(od => Vector( (od(0), od(1)) , (od(1), od(0)) ) )
      .map(od => od -> multipleShortestPathsBuilder.getPaths(vertexCollection(od._1), vertexCollection(od._2), 5)).toMap
      .view
      .mapValues(rc => rc.asScala.toVector.flatMap(r => r.getEdgeList.asScala.toVector.map(_.ID).intersect(amwEdgesIDs.keys.toVector).map(amwEdgesIDs)).distinct)
      .to(Map)
  }



  /**
    * Clones the graph, this should be thread safe and make hard copies of the objects os they can be used for
    * running multiple simulations at once.
    *
    * @param devices The new set of devices to use to make the graph. This way multiple graphs do not share control devices
    * @return Copy of the graph.
    */
  def clone(devices: ControlDevices): RouteGraph = new RouteGraph(
    this.baseVertices, this.standardEdges, this.levelChanges, devices.flowGates, devices.binaryGates, devices.amws, devices.flowSeparators, destinationGroups = this.destinationGroups, beta = this.beta
  )
}
