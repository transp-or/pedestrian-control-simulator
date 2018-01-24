package hubmodel.input.infrastructure

import breeze.linalg.norm
import hubmodel.{Action, PedestrianDES, PedestrianSim, Position, SFGraphSimulator, generateUUID, VertexCell, isInVertex}
import play.api.libs.json.{JsError, JsSuccess, JsValue, Json}
import breeze.linalg.DenseVector
import breeze.numerics.round
import org.jgrapht.alg.shortestpath.DijkstraShortestPath
import org.jgrapht.graph.{DefaultDirectedWeightedGraph, DefaultWeightedEdge}
import scala.io.BufferedSource
import collection.JavaConverters._


/** Representation of an edge used in the graph structure. Can be used as parent for advanced edges with gates.
  *
  * @param startVertex vertex at origin
  * @param endVertex   vertex at destination
  */
class MyEdge(val startVertex: VertexCell, val endVertex: VertexCell) extends DefaultWeightedEdge {

  // ID of the edge
  val ID: String = generateUUID

  // distance between vertices in straight line.
  val length: Double = norm(startVertex.center - endVertex.center)

  // Cost of the edge
  private var _cost: Double = length

  // accessor for the cost
  def cost: Double = _cost

  // setter for the cost. The call to sychronized is to ensure multi-thread correctness
  def updateCost(v: Double): Unit = synchronized(_cost = v)


  /** Checks whether another object equals this one
    *
    * @param other another object to test equality for
    * @return boolean indicating if the two objects are the same
    */
  override def equals(other: Any): Boolean =
    other match {
      case that: MyEdge => that.canEqual(this) && this.startVertex == that.startVertex && this.endVertex == that.endVertex
      case _ => false
    }

  /** Checks whether we are allowed to compare this object to another
    *
    * @param other
    * @return
    */
  def canEqual(other: Any): Boolean = other.isInstanceOf[MyEdge]


  /** Definition of equality.
    *
    * @return Int representing the object
    */
  override def hashCode: Int = {
    (this.startVertex, this.endVertex).##
  }
}

abstract class MyEdgeWithGate(override val startVertex: VertexCell, override val endVertex: VertexCell, val start: Position, val end: Position, val monitoredArea: String) extends MyEdge(startVertex, endVertex) {

  val width: Double = norm(end - start)

  // variable flow rate of the gate [pax/s]
  var flowRate: Double

  // The list of pedestrians which are waiting at the gate to be let through
  val pedestrianQueue: scala.collection.mutable.Queue[PedestrianSim] = scala.collection.mutable.Queue[PedestrianSim]()

  /** Event for releasing a pedestrian. This allows him to pass the gate. Each pedestrian contains state variables
    * indicating whether he is waiting or not. These are used by the other classes.
    *
    * @param sim simulation environment
    */
  class ReleasePedestrian(sim: SFGraphSimulator) extends Action {

    /** Executes the event.
      *
      */
    override def execute(): Unit = {
      sim.eventLogger.trace("sim-time=" + sim.currentTime + ": released pedestrian. Peds in queue=" + pedestrianQueue.size)
      if (pedestrianQueue.nonEmpty) {
        pedestrianQueue.head.isWaiting = false
        pedestrianQueue.head.freedFrom.append(ID)
        pedestrianQueue.dequeue()
      }
      // inserts new event based on the current flow rate allowed through the gate.
      //sim.insertEventWithDelay(1.0 / flowRate)(new ReleasePedestrian(sim))
    }
  }

}

/** Extension of [[hubmodel.input.infrastructure.MyEdge]] for the usage of "flow gates". The gates control the
  * flow of pedestrians passing through them.
  *
  * TODO: A more realistic approach for the modeling of gates could be used to determine the maximum capacity.
  *
  * @param startVertex vertex at origin
  * @param endVertex   vertex at destination
  * @param start       one end of the gate
  * @param end         other end of the gate
  */
class FlowGate(startVertex: VertexCell, endVertex: VertexCell, start: Position, end: Position, ma: String) extends MyEdgeWithGate(startVertex, endVertex, start, end, ma) {

  // variable flow rate of the gate [pax/s]
  var flowRate = 0.5

  /** Checks whether another object equals this one
    *
    * @param other another object to test equality for
    * @return boolean indicating if the two objects are the same
    */
  override def equals(other: Any): Boolean =
    other match {
      case that: FlowGate => super.equals() && that.canEqual(this) && this.start == that.start && this.end == that.end
      case _ => false
    }

  /** Checks whether we are allowed to compare this object to another
    *
    * @param other
    * @return
    */
  override def canEqual(other: Any): Boolean = other.isInstanceOf[FlowGate]


  /** Definition of equality.
    *
    * @return Int representing the object
    */
  override def hashCode: Int = {
    (super.hashCode, this.start, this.end).##
  }
}

/** Object to model gates controlling the flow of pedestrians
  *
  * @param o        vertex "before" the gate
  * @param d        vertex "after" the gate
  * @param start    one end of the gate (used for visualization)
  * @param end      other end of the gate (used for visualization)
  */
class BinaryGate(o: VertexCell,
                 d: VertexCell,
                 start: Position,
                 end: Position,
                 ma: String) extends MyEdgeWithGate(o, d, start, end, ma) {


  var flowRate = Double.MaxValue

  // On creation, all gates are open
  var isOpen: Boolean = true

  /** Checks whether another object equals this one
    *
    * @param other another object to test equality for
    * @return boolean indicating if the two objects are the same
    */
  override def equals(other: Any): Boolean =
    other match {
      case that: BinaryGate => super.equals() && that.canEqual(this) && this.start == that.start && this.end == that.end
      case _ => false
    }

  /** Checks whether we are allowed to compare this object to another
    *
    * @param other
    * @return
    */
  override def canEqual(other: Any): Boolean = other.isInstanceOf[BinaryGate]


  /** Definition of equality.
    *
    * @return Int representing the object
    */
  override def hashCode: Int = {
    (super.hashCode, this.start, this.end).##
  }
}

/** Implementation of moving walkways as an edge. This will be used for the route choice aspects.
  *
  * @param startVertex vertex at origin
  * @param endVertex   vertex at destination
  * @param capacity    capacity of the MV
  */
class MovingWalkway(override val startVertex: VertexCell, override val endVertex: VertexCell, val capacity: Double) extends MyEdge(startVertex, endVertex) {
  val speed: Double = 2.0
}

/** Initialisation of the flow gates. The is the event inserted into the [[SFGraphSimulator.StartSim]] event.
  * The "first round" of the [[hubmodel.input.infrastructure.FlowGate.ReleasePedestrian]] events are inserted;
  * these will call the next events.
  *
  * @param sim simulation environment
  */
class StartFlowGates(sim: SFGraphSimulator) extends Action {
  override def execute(): Unit = {
    sim.eventLogger.trace("sim-time=" + sim.currentTime + ": started flow gates")
    sim.graph.flowGates.foreach(fg => sim.insertEventWithDelay(0)(new fg.ReleasePedestrian(sim)))
  }
}


/** Graph used for the computation of the route choice.
  *
  * Reference for the definition of the equality between two edges and vertices:
  * https://github.com/jgrapht/jgrapht/wiki/EqualsAndHashCode#equalshashcode-for-vertexedge-objects
  *
  * @param vertices      set of vertices
  * @param standardEdges connections between each vertex. THIS SHOULD BE MYEDGES PROBABLY ?
  * @param flowGates     links on which there are flow gates
  */
class RouteGraph(private val vertices: Vector[VertexCell],
                 val standardEdges: Iterable[MyEdge],
                 val flowGates: Iterable[FlowGate],
                 val binaryGates: Iterable[BinaryGate],
                 val movingWalkways: Iterable[MovingWalkway]) extends WithGates {

  def enqueueInWaitingZone(p: PedestrianSim): Unit = super.enqueueInWaitingZone(flowGates)(p)

  // Map from vertex names to the vertices themselves
  val vertexMap: Map[String, VertexCell] = vertices.map(v => v.name -> v).toMap

  // building the graph
  private val network: DefaultDirectedWeightedGraph[VertexCell, MyEdge] = new DefaultDirectedWeightedGraph[VertexCell, MyEdge](classOf[MyEdge])
  vertices.foreach(v => network.addVertex(v))
  val allEdges: Iterable[MyEdge] = standardEdges ++ flowGates ++ binaryGates ++ movingWalkways

  allEdges.foreach(e => {
    network.addEdge(e.startVertex, e.endVertex, e)
    network.setEdgeWeight(e, e.length)
  })

  // object used to get the shortest path in the network
  private var shortestPathBuilder: DijkstraShortestPath[VertexCell, MyEdge] = new DijkstraShortestPath[VertexCell, MyEdge](network)

  /** Updates the cost of each edge in the graph based on the cost of the edges stored in the "edges" variable.
    * This method updates the cost of the edges before actually updating the graph object itslef.
    */
  def updateGraph(): Unit = {
    allEdges.foreach(_.updateCost(1.0))
    allEdges.foreach(e => network.setEdgeWeight(e, e.cost))
    this.shortestPathBuilder = new DijkstraShortestPath[VertexCell, MyEdge](network)
  }

  /** Uses to shortestPathBuilder to compute the shortest path between two vertices.
    *
    * @param o origin node
    * @param d destination node
    * @return the list if vertices representing the path
    */
  def getShortestPath(o: VertexCell, d: VertexCell): List[VertexCell] = {
    shortestPathBuilder.getPath(o, d).getVertexList.asScala.toList
  }

  /** Creates a [[hubmodel.Position]] inside the specific [[hubmodel.VertexCell]] corresponding to the name passed as argument.
    *
    * @param node name of the [[hubmodel.VertexCell]] to generate a point inside
    * @return [[hubmodel.Position]] uniformly sampled inside
    */
  /*def generateInZone(node: String): Position = {
    val rand = breeze.stats.distributions.Uniform(0, 1).sample(2)
    breeze.linalg.DenseVector(vertexMap(node).A(0) + rand(0) * (vertexMap(node).B(0) - vertexMap(node).A(0)), vertexMap(node).A(1) + rand(1) * (vertexMap(node).D(1) - vertexMap(node).A(1)))
  }*/
}

trait WithGates {

  /** Puts a pedestrian in the queue in front of a gate. This method is called by the social force model after each
    * iteration and checks whether the pedestians have entered the queue.
    *
    * @param p pedestrian to enqueue
    */
  def enqueueInWaitingZone(flowGates: Iterable[FlowGate])(p: PedestrianSim): Unit = {
    val gate: Option[FlowGate] = flowGates.filterNot(p.freedFrom contains _.ID).find(fg => isInVertex(fg.startVertex)(p.currentPosition))
    if (gate.isDefined && !gate.get.pedestrianQueue.contains(p)) {
      p.isWaiting = true
      gate.get.pedestrianQueue.enqueue(p)
    }
  }
}

/** Class to read the graph specification file and process it.
  *
  * TODO: convert this class to a function
  *
  * @param graphSpecificationFile JSON file containing the graph specification
  */
class GraphReader(graphSpecificationFile: String) extends Infrastructure {

  override val location: String = "test"
  override val subLocation: String = "test2"

  val graph: RouteGraph = {
    val source: BufferedSource = scala.io.Source.fromFile(graphSpecificationFile)
    val input: JsValue = Json.parse(try source.mkString finally source.close)

    input.validate[InfraGraphParser] match {
      case s: JsSuccess[InfraGraphParser] =>
        val v: Vector[VertexCell] = s.get.nodes.map(n => VertexCell(n.name, DenseVector(n.x1, n.y1), DenseVector(n.x2, n.y2), DenseVector(n.x3, n.y3), DenseVector(n.x4, n.y4)))
        val vertexMap: Map[String, VertexCell] = v.map(v => v.name -> v).toMap
        val e: Iterable[MyEdge] = s.get.standardConnections.flatMap(c => c.conn.map(neigh => new MyEdge(vertexMap(c.node), vertexMap(neigh))))
        val fg: Iterable[FlowGate] = s.get.flowGates.map(fg => new FlowGate(vertexMap(fg.o), vertexMap(fg.d), DenseVector(fg.start_pos_x, fg.start_pos_y), DenseVector(fg.end_pos_x, fg.end_pos_y), fg.area))
        val mv: Iterable[MovingWalkway] = s.get.movingWalkways.map(m => new MovingWalkway(vertexMap(m.o), vertexMap(m.d), 1.0))
        val bg: Iterable[BinaryGate] = s.get.binaryGates.map(bg => new BinaryGate(vertexMap(bg.o), vertexMap(bg.d), DenseVector(bg.s_x, bg.s_y), DenseVector(bg.e_x, bg.e_y), bg.area))
        new RouteGraph(v, e, fg, bg, mv)
      case e: JsError => throw new Error("Error while parsing graph specification file: " + JsError.toJson(e).toString())
    }
  }
}
