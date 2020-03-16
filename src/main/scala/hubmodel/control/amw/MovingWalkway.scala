package hubmodel.control.amw

import hubmodel.DES.{Action, MyEvent, NOMADGraphSimulator, PedestrianDES, PedestrianPrediction}
import hubmodel.control.{ControlDeviceComponent, ControlDevicePolicy}
import hubmodel.supply.continuous.{SINGLELINE, Wall}
import hubmodel.supply.graph.MyEdge
import hubmodel.{Position, Velocity}
import tools.Time
import tools.cells.Vertex

/** Implementation of moving walkways as an edge. This will be used for the route choice aspects.
  *
  * @param startVertex vertex at origin
  * @param endVertex   vertex at destination
  * @param capacity    capacity of the MV
  */
class MovingWalkway(val name: String,
                    override val startVertex: Vertex,
                    override val endVertex: Vertex,
                    val width: Double,
                    val start: Position,
                    val end: Position,
                    val associatedZonesStart: Vector[Vertex],
                    val associatedZonesEnd: Vector[Vertex],
                    val droppedVertices: Vector[String],
                    val associatedConnectivity: Iterable[MyEdge],
                    val parallelFlows: Vector[Vector[Vertex]],
                    val startArea: String,
                   val endArea: String) extends MyEdge(startVertex, endVertex) with ControlDeviceComponent {

  // Direction (considered positive) in which the pedestrians will move when on the moving walkway.
  private val direction: Position = (this.end - this.start).normalized

  // A positive speed makes pedetrians move from start to end a a negative speed makes pedestrians
  // move from end to start.
  private var _speed: Double = 2.0

  /** Getter method for the speed.
    *
    * @return
    */
  def speed: Double = this._speed

  /** Setter method for the speed. Positive means moving from start to end
    * while negative means moving from end to start.
    *
    * @param s new speed of the walkway
    */
  def updateSpeed(s: Double): Unit = this._speed = s

  /** Vectorized moving speed of the walkway
    *
    * @return
    */
  def movingSpeed: Velocity = this.direction * this._speed


  /** The two side walls of the moving walkway.
    *
    * @return
    */
  def walls: Vector[Wall] = {
    val orthDir = (this.end - this.start).orthogonal.normalized
    Vector(
      Wall(this.name+"1", this.start - orthDir * 0.5*width, this.end - orthDir * 0.5*width, SINGLELINE),
      Wall(this.name+"2", this.start + orthDir * 0.5*width, this.end + orthDir * 0.5*width, SINGLELINE)
    )
  }


  private val positiveEdge: MyEdge = new MyEdge(this.startVertex, this.endVertex)
  private val negativeEdge: MyEdge = new MyEdge(this.endVertex, this.startVertex)

  private def updateCosts(): Unit = {
    if (this._speed >= 0) {
      this.positiveEdge.updateCost(math.abs(this.length/(this._speed + math.signum(this._speed) * 1.34)))
      this.negativeEdge.updateCost(Double.MaxValue)
    } else {
      this.positiveEdge.updateCost(Double.MaxValue)
      this.negativeEdge.updateCost(math.abs(this.length/(this._speed + math.signum(this._speed) * 1.34)))
    }
  }

  //this.negativeEdge.updateCost(this.length/(this._speed + 1.34))

  val graphEdges: Vector[MyEdge] = {Vector(positiveEdge, negativeEdge)}

  private val controlPolicy: collection.mutable.PriorityQueue[AMWPolicy] = collection.mutable.PriorityQueue()

  def noControlPolicy: Boolean = this.controlPolicy.isEmpty

  private var nextSpeedUpdate: Option[MyEvent] = None

  def setControlPolicy(policy: Iterable[AMWPolicy]): Unit = {

    this.nextSpeedUpdate.foreach(_.setSkipTrue())

    this.controlPolicy.clear()
    this.controlPolicy.addAll(policy)
  }

  def insertChangeSpeed(sim: NOMADGraphSimulator): Unit = {
    this.nextSpeedUpdate = sim.insertEventAtAbsolute(this.controlPolicy.head.start)(new ChangeAMWSeed(sim))
  }



  class ChangeAMWSeed(sim: NOMADGraphSimulator) extends Action {

    override def execute(): Any = {

      val nextSpeed = controlPolicy.dequeue()

      // Set the new speed for the walkway
      updateSpeed(nextSpeed.speed)

      // update the costs of the graph edges for this walkway
      updateCosts()

      this.sim.graph.updateGraphCosts()

      // Inserts the next event for updating the speed based on the current control policy. This control policy can
      // change so we must be able to find and remove this event from the event list.
      nextSpeedUpdate = {
        if (controlPolicy.nonEmpty) {
          sim.insertEventAtAbsolute(controlPolicy.head.start)(new ChangeAMWSeed(sim))
        }
        else {
          None
        }
      }


    }

    type A = ChangeAMWSeed

    override def deepCopy(simulator: PedestrianPrediction): Option[A] = None
  }




  /** create a deep copy of the control device
    *
    * @return
    */
  override def deepCopy: MovingWalkway = new MovingWalkway(
    this.name,
    this.startVertex,
    this.endVertex,
    this.width,
    this.start,
    this.end,
    this.associatedZonesStart.map(_.deepCopy),
    this.associatedZonesEnd.map(_.deepCopy),
    this.droppedVertices,
    this.associatedConnectivity,
    this.parallelFlows,
    this.startArea,
    this.endArea
  )

  def deepCopyWithState: MovingWalkway = {
   val amw = new MovingWalkway(
      this.name,
      this.startVertex,
      this.endVertex,
      this.width,
      this.start,
      this.end,
      this.associatedZonesStart.map(_.deepCopy),
      this.associatedZonesEnd.map(_.deepCopy),
      this.droppedVertices,
      this.associatedConnectivity,
     this.parallelFlows,
     this.startArea,
     this.endArea
    )
    amw.updateSpeed(this.speed)
    amw.setControlPolicy(this.controlPolicy)
    return amw
  }



}

/** Defintion of speed for a specific time interval for a given AMW. The times are ABSOLUTE for the simulation.
  *
  * @param name
  * @param s
  * @param end
  * @param speed
  */
case class AMWPolicy(name: String, private val s: Time, end: Time, speed: Double) extends ControlDevicePolicy(s) {

  def nameToString: String = this.start + "-" + this.end

  val decisionVariable: Double = this.speed
}

