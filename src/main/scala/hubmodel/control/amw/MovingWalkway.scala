package hubmodel.control.amw

import hubmodel.DES.{Action, MyEvent, NOMADGraphSimulator, PedestrianDES, PedestrianPrediction, PedestrianSimulation}
import hubmodel.control.{ControlDeviceComponent, ControlDeviceData, ControlDevicePolicy}
import hubmodel.ped.PedestrianNOMAD
import hubmodel.route.UpdatePedestrianRoutes
import hubmodel.supply.continuous.{SINGLELINE, Wall}
import hubmodel.supply.graph.MyEdge
import hubmodel.{Position, Velocity}
import tools.Time
import tools.cells.Vertex
import tools.TimeNumeric.mkOrderingOps
import tools.exceptions.ControlDevicesException


/** Implementation of moving walkways as an edge. This will be used for the route choice aspects.
  *
  */
class MovingWalkway(val name: String,
                    val firstVertex: Vertex,
                    val secondVertex: Vertex,
                    val width: Double,
                    val start: Position,
                    val end: Position,
                    val associatedZonesStart: Vector[Vertex],
                    val associatedZonesEnd: Vector[Vertex],
                    val droppedVertices: Vector[String],
                    val associatedConnectivity: Iterable[MyEdge],
                    val parallelFlows: Vector[Vector[Vertex]],
                    val startArea: String,
                    val endArea: String) extends MyEdge(firstVertex, secondVertex) with ControlDeviceComponent {

  outer =>

  // Direction (considered positive) in which the pedestrians will move when on the moving walkway.
  private val direction: Position = (this.end - this.start).normalized


  def startVertex(t: Time): Vertex = {
    if (this.speed(t) >= 0) {this.firstVertex}
    else { this.secondVertex}
  }

  def endVertex(t: Time): Vertex = {
    if (this.speed(t) >= 0) {this.secondVertex}
    else { this.firstVertex}
  }

  /** Getter method for the speed.
    * // A positive speed makes pedetrians move from start to end a a negative speed makes pedestrians
    * // move from end to start.
    *
    * @return
    */
  def speed(t: Time): Double = {
    if (t <= accStartTime) {
      this._previousSpeed
    } else if (t >= accEndTime){
      this._nextSpeed
    } else {
      this._previousSpeed + (t - this.accStartTime).value.toDouble * this.accDirection * this.acc
    }
  }

  /** Setter method for the speed. Positive means moving from start to end
    * while negative means moving from end to start.
    *
    * @param s new speed of the walkway
    */
  def updateSpeed(s: Double): Unit = {
    this._previousSpeed = this._nextSpeed
    this._nextSpeed = s
  }

  private var accStartTime: Time = Time(0)
  private var accEndTime: Time =  Time(0)
  private val acc: Double = hubmodel.AMW_ACCELERATION_AMPLITUDE // norms prescribes max at 1m/ss but in practice is [0.14,0.43]m/ss (Riccardo's paper).
  private var accDirection: Double = 0.0
  private var _nextSpeed: Double = 0.0
  private var _previousSpeed: Double = 0.0



  /** Vectorized moving speed of the walkway
    *
    * @return
    */
  def movingSpeed(t: Time): Velocity = this.direction * this.speed(t)


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


  private val positiveEdge: MyEdge = new MyEdge(this.firstVertex, this.secondVertex)
  private val negativeEdge: MyEdge = new MyEdge(this.secondVertex, this.firstVertex)

  private var isClosed: Boolean = false

  def updateCosts(t: Time): Unit = {
    if (!isClosed) {
      if (this._nextSpeed >= 0) {
        this.positiveEdge.updateCost(t, math.abs(this.length / (math.abs(this._nextSpeed) + 1.34)))
        this.negativeEdge.updateCost(t, Double.PositiveInfinity)
      } else {
        this.positiveEdge.updateCost(t, Double.PositiveInfinity)
        this.negativeEdge.updateCost(t, math.abs(this.length / (math.abs(this._nextSpeed) + 1.34)))
      }
    }
  }

  val graphEdges: Vector[MyEdge] = {Vector(positiveEdge, negativeEdge)}


  private val pedestriansOnAMW: collection.mutable.Set[String] = collection.mutable.Set()
  def addPedToAMW(id: String): Unit = {this.pedestriansOnAMW.addOne(id)}
  def removePedFromAMW(id: String): Unit = {this.pedestriansOnAMW.remove(id)}

  private val controlPolicy: collection.mutable.PriorityQueue[AMWPolicy] =
    collection.mutable.PriorityQueue.empty[(AMWPolicy)](Ordering.by((_: AMWPolicy).start).reverse)

  private val closeTimes: collection.mutable.PriorityQueue[Time] = collection.mutable.PriorityQueue.empty[(Time)](Ordering.by((_: Time).value).reverse)
  private val openTimes: collection.mutable.PriorityQueue[Time] = collection.mutable.PriorityQueue.empty[(Time)](Ordering.by((_: Time).value).reverse)


  val appliedPolicy: collection.mutable.ArrayBuffer[(Time, Double)] = collection.mutable.ArrayBuffer()

  val expectedPolicy: collection.mutable.ArrayBuffer[Vector[(Time, Double)]] = collection.mutable.ArrayBuffer()

  def noControlPolicy: Boolean = this.controlPolicy.isEmpty

  private var nextSpeedUpdate: Option[MyEvent] = None
  private val nextCloseAMW: collection.mutable.ArrayBuffer[MyEvent] = collection.mutable.ArrayBuffer()
  private val nextOpenAMW: collection.mutable.ArrayBuffer[MyEvent] = collection.mutable.ArrayBuffer()

  def setControlPolicy(policy: Vector[AMWPolicy], events: Option[MovingWalkwayControlEvents]): Unit = {

    if (policy.exists(_.name != this.name)) {
      throw new IllegalArgumentException("AMW policy has policy from different control device")
    }

    /*if (policy.nonEmpty) {
      this.expectedPolicy.append(
        (policy.minBy(_.start).start.value until policy.maxBy(_.end).end.value by 0.5).map(t => (Time(t.toDouble), this.speed(Time(t.toDouble)))).toVector
      )
    }*/

    this.nextSpeedUpdate.foreach(_.setSkipTrue())
    this.nextOpenAMW.foreach(_.setSkipTrue())
    this.nextCloseAMW.foreach(_.setSkipTrue())

    this.controlPolicy.clear()
    this.controlPolicy.addAll(policy)

    events.foreach(e => {
      this.closeTimes.clear()
      this.openTimes.clear()

      this.closeTimes.addAll(e.closeTime)
      this.openTimes.addAll(e.openTime)
    })
  }

  def insertChangeSpeed(sim: NOMADGraphSimulator): Unit = {
    this.nextSpeedUpdate = sim.insertEventAtAbsolute(this.controlPolicy.head.start)(new ChangeAMWSpeed(sim))
  }


  private class OpenAMW(sim: NOMADGraphSimulator) extends Action {

    override def execute(): Any = {

      outer.isClosed = false

      // make pedestrians backtrack to the entrance of the amw since it's now moving in the other direction.
      outer.pedestriansOnAMW.foreach(id => {
        val ped: Option[PedestrianNOMAD] = this.sim.pedByID(id)
        ped match {
          case Some(p) => {
            p.reverseRoute()
            //p.baseVelocity = outer.movingSpeed
          }
          case None => {
            throw new ControlDevicesException("Error when opening AMW ! Pedestrian not in population: " + id)
          }
        }
        this.sim.pedByID(id)
      })

      updateCosts(sim.currentTime)
      this.sim.insertEventWithZeroDelay(new UpdatePedestrianRoutes(this.sim, false))
    }

    type A = OpenAMW
    override def deepCopy(simulator: PedestrianPrediction): Option[A] = None
  }

  private class CloseAMW(sim: NOMADGraphSimulator) extends Action {

    override def execute(): Any = {
      outer.isClosed = true
      outer.positiveEdge.updateCost(sim.currentTime, Double.PositiveInfinity)
      outer.negativeEdge.updateCost(sim.currentTime, Double.PositiveInfinity)
      this.sim.insertEventWithZeroDelay(new UpdatePedestrianRoutes(this.sim, false))
    }

    type A = CloseAMW
    override def deepCopy(simulator: PedestrianPrediction): Option[A] = None
  }



  private class ChangeAMWSpeed(sim: NOMADGraphSimulator) extends Action {

    override def execute(): Any = {

      val speedToSet: AMWPolicy = outer.controlPolicy.dequeue()

      //if (sim.verbose) {println("old speed for " + speedToSet.name + "@" + sim.currentTime  + ": " + _nextSpeed)}

      // Set the new speed for the walkway
      outer.updateSpeed(speedToSet.speed)

      outer.accStartTime = speedToSet.start
      outer.accEndTime = speedToSet.start + Time(math.abs(outer._nextSpeed - outer._previousSpeed)/outer.acc)
      outer.accDirection = {
        if (outer._nextSpeed == outer._previousSpeed) {
          0.0
        }
        else if (outer._nextSpeed > outer._previousSpeed) {
          1.0
        }
        else {
          -1.0
        }
      }

      if (outer.closeTimes.nonEmpty) {
        sim.insertEventAtAbsolute(outer.closeTimes.dequeue())(new CloseAMW(this.sim)).foreach(nextCloseAMW.addOne)
        sim.insertEventAtAbsolute(outer.openTimes.dequeue())(new OpenAMW(this.sim)).foreach(nextOpenAMW.addOne)
      }

      /*if ((outer._previousSpeed.sign == -1 && outer._nextSpeed.sign == 1) || (outer._previousSpeed.sign == 1 && outer._nextSpeed.sign == -1)) {
        nextCloseAMW = sim.insertEventWithZeroDelay(new CloseAMW(this.sim))
        nextOpenAMW = sim.insertEventWithDelay(Time((outer.length / 1.34) + math.abs(outer._previousSpeed/outer.acc)))(new OpenAMW(this.sim))
      }*/


      // update the costs of the graph edges for this walkway
      outer.updateCosts(this.sim.currentTime)
      this.sim.insertEventWithZeroDelay(new UpdatePedestrianRoutes(this.sim, false))

      // Inserts the next event for updating the speed based on the current control policy. This control policy can
      // change so we must be able to find and remove this event from the event list.
      nextSpeedUpdate = {
        if (controlPolicy.nonEmpty) {
          sim.insertEventAtAbsolute(controlPolicy.head.start)(new ChangeAMWSpeed(sim))
        }
        else {
          None
        }
      }

        /*if (sim.verbose) {
        println("new speed for " + speedToSet.name + " @ " + sim.currentTime + ": " + _nextSpeed)
      }*/
    }

    type A = ChangeAMWSpeed

    override def deepCopy(simulator: PedestrianPrediction): Option[A] = None
  }




  /** create a deep copy of the control device
    *
    * @return
    */
  override def deepCopy: MovingWalkway = new MovingWalkway(
    this.name,
    this.firstVertex,
    this.secondVertex,
    this.width,
    this.start,
    this.end,
    this.associatedZonesStart.map(_.deepCopy),
    this.associatedZonesEnd.map(_.deepCopy),
    this.droppedVertices,
    this.associatedConnectivity.map(_.deepCopy),
    this.parallelFlows,
    this.startArea,
    this.endArea
  )

  def deepCopyWithState(pop: Iterable[PedestrianNOMAD]): MovingWalkway = {
   val amw = new MovingWalkway(
      this.name,
      this.firstVertex,
      this.secondVertex,
      this.width,
      this.start,
      this.end,
      this.associatedZonesStart.map(_.deepCopy),
      this.associatedZonesEnd.map(_.deepCopy),
      this.droppedVertices,
      this.associatedConnectivity.map(_.deepCopy),
     this.parallelFlows,
     this.startArea,
     this.endArea
    )
    amw._previousSpeed = this._previousSpeed
    amw._nextSpeed = this._nextSpeed
    amw.accDirection = this.accDirection
    amw.accStartTime = this.accStartTime
    amw.accEndTime = this.accEndTime
    amw.setControlPolicy(this.controlPolicy.toVector, if (this.closeTimes.nonEmpty) {Some(MovingWalkwayControlEvents(this.name, this.closeTimes.toVector, this.openTimes.toVector))} else {None})
    amw.pedestriansOnAMW.addAll(pop.filter(p => p.isInsideAMW.isDefined && p.isInsideAMW.get == amw.name).map(_.ID))
    return amw
  }

  override def toJSON: String = {
    "{\n" +
      "\"ID\":\"" + this.ID + "\",\n" +
      "\"name\":\"" + this.name + "\",\n" +
      "\"start_vertex\":\"" + this.firstVertex.name + "\",\n" +
      "\"end_vertex\":\"" + this.secondVertex.name + "\",\n" +
      "\"applied_speed_history\":[" + this.appliedPolicy.map(tc => "[" + tc._1 + "," + tc._2 + "]").mkString(",\n") + "],\n"+
      "\"expected_speed_history\":[[" + this.expectedPolicy.map(p => "[" + p.map(tc => tc._1 + "," + tc._2).mkString("],[") + "]").mkString("],[") + "]]\n"+
      "}"
  }
}

/** Defintion of speed for a specific time interval for a given AMW. The times are ABSOLUTE for the simulation.
  *
  * @param _n
  * @param _s
  * @param end
  * @param speed
  */
case class AMWPolicy(private val _n: String, private val _s: Time, end: Time, speed: Double, amwLength: Double) extends ControlDevicePolicy(_s, _n) {

  def nameToString: String = this.name + "-" + this.start + "-" + this.end

  val decisionVariable: Double = this.speed
}


case class MovingWalkwayControlEvents(private val __n: String, closeTime: Vector[Time] = Vector(), openTime: Vector[Time] = Vector()) extends ControlDeviceData(__n)

