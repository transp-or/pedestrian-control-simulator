package hubmodel.supply.graph

import java.util.concurrent.ThreadLocalRandom

import hubmodel.mgmt.ControlDevices
import hubmodel.mgmt.flowsep.FlowSeparator
import hubmodel.ped.PedestrianNOMAD
import hubmodel.tools.cells.Rectangle

class MultipleGraph( fg: Iterable[FlowGate],
                     bg: Iterable[BinaryGate],
                     mw: Iterable[MovingWalkway],
                     fs: Iterable[FlowSeparator]
                   ) extends GraphContainer(fg, bg, mw, fs) {


  type PopulationFraction = Double
  type AlternativeGraph = (PopulationFraction, RouteGraph)

  // Collection of graphs to choose from
  private val _graphCollection: collection.mutable.Map[String, AlternativeGraph] = collection.mutable.Map()

  // Accessor to the graph collection
 /* def graphs: Map[String, AlternativeGraph] = {
    this._graphCollection.toMap
  }

  // Get the list of graph IDs
  def graphID: collection.Set[String] = {
    this._graphCollection.keySet
  }

  // Accessor to one specific graph by ID
  def graph(id: String): AlternativeGraph = {
    this._graphCollection(id)
  }*/

  // Adds a new [[RouteGraph]] object to the collection.
  def addGraph(id: String, frac: PopulationFraction, vertices: Iterable[Rectangle], edges: Iterable[MyEdge], edges2Add: Set[MyEdge], edges2Remove: Set[MyEdge], lc: Iterable[MyEdgeLevelChange]): Unit = {
    if (this._graphCollection.keySet.contains(id)) {
      throw new Exception("ID is not unique for graph ! " + id)
    }
    else {
      this._graphCollection += id -> (frac, new RouteGraph(vertices, edges, lc , this.flowGates, this.binaryGates, this.movingWalkways, this.flowSeparators, edges2Add, edges2Remove))
    }
  }

  def processIntermediateArrival(ped: PedestrianNOMAD): Unit = {
    this._graphCollection(ped.graph)._2.processIntermediateArrival(ped)
  }

  // Get the vertex map from all the graphs.
  def vertexMapNew: Map[String, Rectangle] = this._graphCollection.flatMap(_._2._2.vertexCollection).toMap

  // Get set of all edges
  def edges: Set[MyEdge] = this._graphCollection.flatMap(_._2._2.edgeCollection).toSet


  private def sampleGraphs: String = {
    val vectorizedIds: Vector[(String, Double)] = this._graphCollection.map(v => (v._1, v._2._1)).toVector
    val bins: Vector[Double] = this._graphCollection.map(v => (v._1, v._2._1)).toVector.scanLeft(0.0)(_ + _._2).tail
    vectorizedIds(bins.indexWhere(_ > ThreadLocalRandom.current.nextDouble(0.0, 1.0)))._1
  }

  def setRouteFirst(ped: PedestrianNOMAD): Unit = {
    ped.setGraph(this.sampleGraphs)
    this.processIntermediateArrival(ped)
  }

  type T = MultipleGraph
  /**
    * Clones the graph, this should be thread safe and make hard copies of the objects os they can be used for
    * running multiple simulations at once.
    *
    * @param devices The new set of devices to use to make the graph. This way multiple graphs do not share control devices
    * @return Copy of the graph.
    */
  def clone(devices: ControlDevices): T = {

    val graphs = new MultipleGraph(devices.flowGates, devices.binaryGates, devices.amws, devices.flowSeparators)
    this._graphCollection.foreach(g => {
      graphs.addGraph(g._1, g._2._1, g._2._2.vertexCollection.values, g._2._2.edgeCollection, Set(), Set(), g._2._2.levelChanges)
    })
    graphs
  }

}
