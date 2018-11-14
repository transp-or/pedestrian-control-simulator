package hubmodel.supply.graph

import hubmodel.mgmt.ControlDevices
import hubmodel.mgmt.flowsep.FlowSeparator
import hubmodel.ped.{PedestrianNOMAD, PedestrianNOMADWithGraph, PedestrianSim}
import hubmodel.tools.cells.Rectangle

import scala.util.{Failure, Success, Try}

/** Container for multiple gaphs in the same network. The graphs must be populated before the instance is passed to
  * the main simulator. The graphs are stored in a mutable map with strings as keys (IDs). Only an immutable copy can be
  * accessed from outside.
  *
  * @param levelChanges
  * @param flowGates
  * @param binaryGates
  * @param movingWalkways
  * @param flowSeparators
  * @tparam T
  */
class RouteGraphMultiple[T <: PedestrianNOMADWithGraph](private val levelChanges: Iterable[MyEdgeLevelChange],
                                                        private val flowGates: Iterable[FlowGate],
                                                        private val binaryGates: Iterable[BinaryGate],
                                                        private val movingWalkways: Iterable[MovingWalkway],
                                                        private val flowSeparators: Iterable[FlowSeparator]) extends RouteGraphParent[T] {

  // Function to enqueue pedestrians into queues at gates.
  /*def enqueueInWaitingZone(p: T): Unit = {
    super.enqueueInWaitingZoneGen(flowGates)(p)
  }*/

  // Collection of graphs to choose from
  private var _graphCollection: collection.mutable.Map[String, RouteGraph[T]] = collection.mutable.Map()

  // Accessor to the graph collection
  def graphs: Map[String, RouteGraph[T]] = { this._graphCollection.toMap }

  // Get the list of graph IDs
  def graphID: collection.Set[String] = { this._graphCollection.keySet }

  // Accessor to one specific graph by ID
  def graph(id: String): RouteGraph[T] = { this._graphCollection(id) }

  // Adds a new [[RouteGraph]] object to the collection.
  def addGraph(id: String, v: Iterable[Rectangle], e: Iterable[MyEdge]): Unit = {
    if (this._graphCollection.keySet.contains(id)) {
      throw new Exception("ID is not unique for graph ! " + id)
    }
    else { this._graphCollection += id -> new RouteGraph[T](v, e, levelChanges, this.flowGates, this.binaryGates, this.movingWalkways, this.flowSeparators) }
  }

  type V = PedestrianNOMADWithGraph

  // Processes the intermediate arrivals. This method calls the method on the graph which the pedestrian is following.
  def processIntermediateArrival(ped: T): Unit = {
    this._graphCollection(ped.graph).processIntermediateArrival(ped)
  }

  // Get the vertex map from all the graphs.
  def vertexMap: Map[String, Rectangle] = this._graphCollection.flatMap(_._2.vertexMap).toMap

  // Get set of all edges
  def edges: Set[MyEdge] = this._graphCollection.flatMap(_._2.edges).toSet

  def getShortestPath(o: Rectangle, d: Rectangle, ID: Option[String] = None): List[Rectangle] = {
    this._graphCollection(ID.get).getShortestPath(o, d)
  }

  def clone(devices: ControlDevices): RouteGraphMultiple[T] = {
    val newGraphs = new RouteGraphMultiple[T](
      levelChanges,
      flowGates,
      binaryGates,
      movingWalkways,
      flowSeparators
    )

    this._graphCollection.foreach(g => {
      newGraphs.addGraph(g._1, g._2.vertices, g._2.edges)
    })

    newGraphs
  }
}
