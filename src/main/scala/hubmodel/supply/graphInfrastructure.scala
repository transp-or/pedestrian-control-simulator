package hubmodel.supply

import breeze.linalg.{DenseVector, norm}
import hubmodel.{NewBetterPosition2D, _}
import org.jgrapht.alg.shortestpath.DijkstraShortestPath
import org.jgrapht.graph.{DefaultDirectedWeightedGraph, DefaultWeightedEdge}
import play.api.libs.json.{JsError, JsSuccess, JsValue, Json}
import myscala.math.vector.Vector2D

import scala.collection.JavaConverters._
import scala.io.BufferedSource


/** Representation of an edge used in the graph structure. Can be used as parent for advanced edges with gates.
  *
  * @param startVertex vertex at origin
  * @param endVertex   vertex at destination
  */
class MyEdge(val startVertex: VertexRectangle, val endVertex: VertexRectangle) extends DefaultWeightedEdge {

  // ID of the edge
  val ID: String = generateUUID

  // distance between vertices in straight line.
  val length: Double = (startVertex.center - endVertex.center).norm

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

abstract class MyEdgeWithGate(override val startVertex: VertexRectangle, override val endVertex: VertexRectangle, val start: Position, val end: Position, val monitoredArea: String) extends MyEdge(startVertex, endVertex) {

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
      if (pedestrianQueue.nonEmpty) {
        pedestrianQueue.head.isWaiting = false
        pedestrianQueue.head.freedFrom.append(ID)
        pedestrianQueue.dequeue()
        sim.eventLogger.trace("sim-time=" + sim.currentTime + ": gate: " + this + ": released pedestrian. Peds in queue=" + pedestrianQueue.size)
      } else {
        sim.eventLogger.trace("sim-time=" + sim.currentTime + ": gate: " + this + ": no one in queue to release")
      }
      // inserts new event based on the current flow rate allowed through the gate.
      //sim.insertEventWithDelay(1.0 / flowRate)(new ReleasePedestrian(sim))
    }
  }

  /** Enqueues a pedestrian i the queue for passing through a flow gate.
    *
    * @param sim
    */
  class EnqueuePedestrian(ped: PedestrianSim, sim: SFGraphSimulator) extends Action {

    override def execute(): Unit = {
      ped.isWaiting = true
      pedestrianQueue.enqueue(ped)
      sim.eventLogger.trace("sim-time=" + sim.currentTime + ": enqueued pedestrian. Peds in queue=" + pedestrianQueue.size)
    }
  }

}

/** Extension of [[hubmodel.supply.MyEdge]] for the usage of "flow gates". The gates control the
  * flow of pedestrians passing through them.
  *
  * TODO: A more realistic approach for the modeling of gates could be used to determine the maximum capacity.
  *
  * @param startVertex vertex at origin
  * @param endVertex   vertex at destination
  * @param start       one end of the gate
  * @param end         other end of the gate
  */
class FlowGate(startVertex: VertexRectangle, endVertex: VertexRectangle, start: Position, end: Position, ma: String) extends MyEdgeWithGate(startVertex, endVertex, start, end, ma) {

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

  override def toString: String = "FlowGate: o: " + startVertex + ", d:" + endVertex
}

/** Object to model gates controlling the flow of pedestrians
  *
  * @param o        vertex "before" the gate
  * @param d        vertex "after" the gate
  * @param start    one end of the gate (used for visualization)
  * @param end      other end of the gate (used for visualization)
  */
class BinaryGate(o: VertexRectangle,
                 d: VertexRectangle,
                 start: Position,
                 end: Position,
                 ma: String) extends MyEdgeWithGate(o, d, start, end, ma) {


  var flowRate: Double = Double.MaxValue

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
class MovingWalkway(override val startVertex: VertexRectangle, override val endVertex: VertexRectangle, val capacity: Double) extends MyEdge(startVertex, endVertex) {
  val speed: Double = 2.0
}

/** Initialisation of the flow gates. The is the event inserted into the [[SFGraphSimulator.StartSim]] event.
  * The "first round" of the [[hubmodel.supply.FlowGate.ReleasePedestrian]] events are inserted;
  * these will call the next events.
  *
  * @param sim simulation environment
  */
class StartFlowGates(sim: SFGraphSimulator) extends Action {
  override def execute(): Unit = {
    sim.eventLogger.trace("sim-time=" + sim.currentTime + ": started flow gates")
    sim.controlDevices.flowGates.foreach(fg => sim.insertEventWithZeroDelay(new fg.ReleasePedestrian(sim)))
  }
}



trait WithGates {

  /** Puts a pedestrian in the queue in front of a gate. This method is called by the social force model after each
    * iteration and checks whether the pedestians have entered the queue.
    *
    * @param p pedestrian to enqueue
    */
  def enqueueInWaitingZone(flowGates: Iterable[FlowGate])(p: PedestrianSim): Unit = {
    val gate: Option[FlowGate] = flowGates.filterNot(p.freedFrom contains _.ID).find(fg => isInVertex(fg.startVertex)(p.currentPositionNew))
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
class GraphReader(graphSpecificationFile: String,
                  useFlowGates: Boolean,
                  useBinarygates: Boolean,
                  useAMWs: Boolean,
                  measureDensity: Boolean) extends Infrastructure {

  override val location: String = "test"
  override val subLocation: String = "test2"

  val (graph, flowGates, binaryGates, amws, monitoredAreas): (RouteGraph, Iterable[FlowGate], Iterable[BinaryGate], Iterable[MovingWalkway], Iterable[VertexRectangle]) = {
    val source: BufferedSource = scala.io.Source.fromFile(graphSpecificationFile)
    val input: JsValue = Json.parse(try source.mkString finally source.close)

    input.validate[InfraGraphParser] match {
      case s: JsSuccess[InfraGraphParser] =>
        val v: Vector[VertexRectangle] = s.get.nodes.map(n => VertexRectangle(n.name, Vector2D(n.x1, n.y1), Vector2D(n.x2, n.y2), Vector2D(n.x3, n.y3), Vector2D(n.x4, n.y4)))
        val vertexMap: Map[String, VertexRectangle] = v.map(v => v.name -> v).toMap
        val e: Iterable[MyEdge] = s.get.standardConnections.flatMap(c => c.conn.map(neigh => new MyEdge(vertexMap(c.node), vertexMap(neigh))))
        val fg: Iterable[FlowGate] = if (useFlowGates) {
          s.get.flowGates.map(fg => new FlowGate(vertexMap(fg.o), vertexMap(fg.d), DenseVector(fg.start_pos_x, fg.start_pos_y), DenseVector(fg.end_pos_x, fg.end_pos_y), fg.area))
        } else {
          Vector()
        }
        val mv: Iterable[MovingWalkway] = if (useAMWs) {
          s.get.movingWalkways.map(m => new MovingWalkway(vertexMap(m.o), vertexMap(m.d), 1.0))
        } else {
          Vector()
        }
        val bg: Iterable[BinaryGate] = if (useBinarygates) {
          s.get.binaryGates.map(bg => new BinaryGate(vertexMap(bg.o), vertexMap(bg.d), DenseVector(bg.s_x, bg.s_y), DenseVector(bg.e_x, bg.e_y), bg.area))
        } else {
          Vector()
        }
        val monitoredAreas: Iterable[VertexRectangle] = if (measureDensity) {
          s.get.controlledAreas.map(ma => VertexRectangle(ma.name, new NewBetterPosition2D(ma.x1, ma.y1), new NewBetterPosition2D(ma.x2, ma.y2), new NewBetterPosition2D(ma.x3, ma.y3), new NewBetterPosition2D(ma.x4, ma.y4)))
        } else {
          Vector()
        }
        (new RouteGraph(v, e, fg, bg, mv), fg, bg, mv, monitoredAreas)
      case e: JsError => throw new Error("Error while parsing graph specification file: " + JsError.toJson(e).toString())
    }
  }
}
