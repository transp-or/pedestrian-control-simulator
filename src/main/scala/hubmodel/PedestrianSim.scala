package hubmodel

import java.util.concurrent.ThreadLocalRandom

import breeze.linalg.DenseVector
import breeze.numerics.{floor, round}
import hubmodel.input.infrastructure.{FlowGate, NodeID}

/**
  * Created by nicholas on 5/13/17.
  */


/** Pedestrian used for the social force model and the network based route choice model.
  *
  * @param oZone origin zone of the pedestrian
  * @param dZone destination zone of the pedestrian
  * @param freeFlowVel free flow speed of the individual
  * @param entryTime entry time into the system
  * @param currentPosition current position (general point)
  * @param currentDestination intermediate destination point (next target)
  * @param route initial route
  */
class PedestrianSim(val oZone: Int,
                    val dZone: Int,
                    val freeFlowVel: Double,
                    val entryTime: Time,
                    var currentPosition: Position,
                    var currentDestination: Position,
                    var route: List[Vertex]) extends PedestrianTrait {

  assert(freeFlowVel > 0.0, "Unacceptable free flow velocity")

  /** Overloaded constructor where the [[freeFlowVel]] is sampled from Weidmann's distribution.
    *
    * @param oZone origin zone of the pedestrian
    * @param dZone destination zone of the pedestrian
    * @param entryTime entry time into the system
    * @param posO current position (general point)
    * @param posD intermediate destination point (next target)
    * @param route initial route
    */
  def this(oZone: Int, dZone: Int, entryTime: Time, posO: Position, posD: Position, route: List[Vertex])
  {
    this(oZone, dZone, 1.34+0.0*ThreadLocalRandom.current().nextGaussian(), entryTime, posO, posD, route)
  }

  /** current velocity, initialized to 0 */
  var currentVelocity: Velocity = DenseVector(0.0,0.0)

  /** total travel time */
  var travelTime: Double = 0.0

  /** total travelled distance */
  var travelDistance: Double = 0.0

  /** exit time from the system */
  var exitTime: Time = 0.0

  val r: Double = ThreadLocalRandom.current.nextDouble(0.25, 0.35)
  var omega: Double = 0.0
  var theta: Double = 0.0
  val m: Double = ThreadLocalRandom.current.nextDouble(60.0, 90.0) // mass of the pedestrian
  val I: Double = 0.5 * r * r * m


  /** is the pedestrian waiting in a zone */
  var isWaiting: Boolean = false

  /** collection of zones whichi must not block the pedestrian (make him wait) */
  val freedFrom: scala.collection.mutable.ArrayBuffer[String] = scala.collection.mutable.ArrayBuffer()

  /** History of the pedestrians positions */
  private var _historyPosition: Vector[(Time, Position)] = Vector((entryTime, currentPosition))

  /** target zone */
  var nextZone: Vertex = route.head

  /** getter methdd for the history of the positions */
  def getHistoryPosition: Vector[(Time, Position)] = _historyPosition

  /** Adds a specific position to the history vector */
  def addHistory(t: Time, pos: Position): Unit = _historyPosition = _historyPosition :+ (t, currentPosition)

  /** Adds the current position (currentPosition) to the history */
  def addHistory(t: Time): Unit = _historyPosition = _historyPosition :+ (t, this.currentPosition)

  /*def popHistory: Position = {
    //if (_historyPosition.isEmpty) None
    //else {
    val ret = _historyPosition.head._2
    _historyPosition = _historyPosition.tail
    ret
    //}
  }*/

  /* Position increment to be added at the next time interval */
  var positionIncrement: Position = DenseVector(0.0,0.0)

  /* Velocity increment to be added at the next time inteval */
  var velocityIncrement: Velocity = DenseVector(0.0,0.0)

  /** Moves each pedestrian with the already compuoted increments
    * The increments are set by the [[hubmodel.mvmtmodels.SocialForceESI]] model.
    */
  def move(): Unit = {
    this.currentPosition = this.currentPosition + this.positionIncrement
    this.positionIncrement = DenseVector(0.0, 0.0)
    this.currentVelocity = boundVelocity(this.currentVelocity + this.velocityIncrement)
    this.velocityIncrement = DenseVector(0.0, 0.0)
  }

  /*
  /** Moves a pedestrian using the headed social force model.
    *
    * @param posIncr increment in position
    * @param velIncr increment in velocity
    * @param headed increment in rotation
    */
  def moveHeaded(posIncr: Position, velIncr: Velocity, headed: DenseVector[Double]): Unit = {
    this.currentPosition = this.currentPosition + posIncr
    this.currentVelocity = boundVelocity(this.currentVelocity + velIncr)
    this.theta = this.theta + headed(0)
    this.omega = this.omega + headed(1)
  }
*/

  /** Converts a pedestrian to a string
    *
    * @return pedestrian printed as a string with some information.
    */
  override def toString: String = { this.ID + ", O=" + this.oZone.toString + ", pos=" + this.currentPosition.toString + ", D=" + this.dZone.toString + ", vel=" + this.currentVelocity }

  /** Prints all the history of the pedestrian in the VisioSafe format. This way, any code which works with VS also
    * works on the results from the simulations.
    * @param refDate year, month and day to print
    * @return one row per position in the historyPosition
    */
  def toVisioSafeFormat(refDate: String = "2013,1,1"): String = {
    def innerPrint(hist: Vector[(Time, Position)], str: String): String = {
      if (hist.isEmpty) str
      else if (str.isEmpty) innerPrint(hist.tail, refDate + "," + time2VisioSafeTime(hist.head._1) + ",0," + round(hist.head._2(0) * 1000) + "," + round(hist.head._2(1) * 1000) + "," + this.ID.hashCode)
      else innerPrint(hist.tail, str + "\n" + refDate + "," + time2VisioSafeTime(hist.head._1) + ",0," + round(hist.head._2(0) * 1000) + "," + round(hist.head._2(1) * 1000) + "," + this.ID.hashCode)
    }
    innerPrint(this._historyPosition, "")
  }
}




