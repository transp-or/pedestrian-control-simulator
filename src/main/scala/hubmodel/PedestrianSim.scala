package hubmodel

import java.util.concurrent.ThreadLocalRandom

import breeze.linalg.DenseVector
import breeze.numerics.round

import myscala.math.vector.ZeroVector2D


/**
  * Created by nicholas on 5/13/17.
  */


/** Pedestrian used for the social force model and the network based route choice model.
  *
  * @param oZone              origin zone of the pedestrian
  * @param dZone              destination zone of the pedestrian
  * @param freeFlowVel        free flow speed of the individual
  * @param entryTime          entry time into the system
  * @param currentPosition    current position (general point)
  * @param currentDestination intermediate destination point (next target)
  * @param route              initial route
  */
class PedestrianSim(val oZone: VertexRectangle,
                    val dZone: VertexRectangle,
                    val freeFlowVel: Double,
                    val entryTime: NewTime,
                    var currentPosition: NewBetterPosition2D,
                    var currentDestination: NewBetterPosition2D,
                    var route: List[VertexRectangle]) extends PedestrianTrait {

  var currentPositionNew: NewBetterPosition2D = currentPosition//new NewBetterPosition2D(currentPosition(0), currentPosition(1))
  var currentDestinationNew: NewBetterPosition2D = currentDestination//new NewBetterPosition2D(currentDestination(0), currentDestination(1))

  assert(freeFlowVel > 0.0, "Unacceptable free flow velocity")

  /** Overloaded constructor where the [[freeFlowVel]] is sampled from Weidmann's distribution.
    *
    * @param oZone     origin zone of the pedestrian
    * @param dZone     destination zone of the pedestrian
    * @param entryTime entry time into the system
    * @param posO      current position (general point)
    * @param posD      intermediate destination point (next target)
    * @param route     initial route
    */
  def this(oZone: VertexRectangle, dZone: VertexRectangle, entryTime: NewTime, posO: NewBetterPosition2D, posD: NewBetterPosition2D, route: List[VertexRectangle]) {
    this(oZone, dZone, 1.34 + 0.0 * ThreadLocalRandom.current().nextGaussian(), entryTime, posO, posD, route)
  }

  /*def this(oZone: VertexRectangle, dZone: VertexRectangle, entryTime: NewTime, posO: NewBetterPosition2D, posD: NewBetterPosition2D, route: List[VertexRectangle]) {
    this(oZone, dZone, 1.34 + 0.0 * ThreadLocalRandom.current().nextGaussian(), entryTime, DenseVector(posO.X, posO.Y), DenseVector(posD.X, posD.Y), route)
  }*/

  /** current velocity, initialized to 0 */
  //var currentVelocity: Velocity = DenseVector(0.0, 0.0)
  var currentVelocityNew: NewBetterVelocity2D = new ZeroVector2D

  /** total travel time */
  var travelTime: NewTime = NewTime(0.0)

  /** total travelled distance */
  var travelDistance: Double = 0.0

  /** exit time from the system */
  var exitTime: NewTime = NewTime(0.0)

  val r: Double = ThreadLocalRandom.current.nextDouble(0.2, 0.3)
  var omega: Double = 0.0
  var theta: Double = 0.0
  val m: Double = ThreadLocalRandom.current.nextDouble(60.0, 90.0) // mass of the pedestrian
  val I: Double = 0.5 * r * r * m

  val a0: Double = ThreadLocalRandom.current.nextDouble(8.0 , 12.0)
  val r0: Double = ThreadLocalRandom.current.nextDouble(0.10, 0.6 )
  val a1: Double = ThreadLocalRandom.current.nextDouble(8.0 , 12.0)
  val r1: Double = ThreadLocalRandom.current.nextDouble(0.10, 0.6 )
  val k0: Double = ThreadLocalRandom.current.nextDouble(800.0,1200.0)
  val k1: Double = ThreadLocalRandom.current.nextDouble(800.0,1200.0)

  /** is the pedestrian waiting in a zone */
  var isWaiting: Boolean = false

  /** collection of zones whichi must not block the pedestrian (make him wait) */
  val freedFrom: scala.collection.mutable.ArrayBuffer[String] = scala.collection.mutable.ArrayBuffer()

  /** History of the pedestrians positions */
  //private var _historyPosition: Vector[(Time, Position)] = Vector((entryTime, currentPosition))
  private var _historyPosition: Vector[(NewTime, NewBetterPosition2D)] = Vector((entryTime, currentPositionNew))

  /** target zone */
  var nextZone: VertexRectangle = route.head

  /** getter methdd for the history of the positions */
  def getHistoryPosition: Vector[(NewTime, NewBetterPosition2D)] = _historyPosition

  /** Adds a specific position to the history vector */
  /*def addHistory(t: Time, pos: Position): Unit = {
    _historyPosition = _historyPosition :+ (NewTime(t), this.currentPositionNew)
  }*/

  /** Adds the current position (currentPosition) to the history */
  def addHistory(t: NewTime): Unit = {
    _historyPosition = _historyPosition :+ (t, this.currentPositionNew)
    //_historyPosition = _historyPosition :+ (new NewTime(t), this.currentPositionNew)
  }

  def setCurrentDestination(pos: NewBetterPosition2D): Unit = {
    this.currentDestination = pos
    this.currentDestinationNew = pos
  }

  /*def popHistory: Position = {
    //if (_historyPosition.isEmpty) None
    //else {
    val ret = _historyPosition.head._2
    _historyPosition = _historyPosition.tail
    ret
    //}
  }*/

  /* Position increment to be added at the next time interval */
  //var positionIncrement: Position = DenseVector(0.0, 0.0)

  /* Velocity increment to be added at the next time inteval */
  //var velocityIncrement: Velocity = DenseVector(0.0, 0.0)

  /* Position increment to be added at the next time interval */
  var positionIncrementNew: NewBetterPosition2D = new ZeroVector2D

  /* Velocity increment to be added at the next time inteval */
  var velocityIncrementNew: NewBetterVelocity2D = new ZeroVector2D

  /** Moves each pedestrian with the already compuoted increments
    * The increments are set by the [[hubmodel.mvmtmodels.SocialForceESI]] model.
    */
  def move(): Unit = {
    //this.currentPosition = this.currentPosition + this.positionIncrement
    this.currentPositionNew = this.currentPositionNew + positionIncrementNew
    this.positionIncrementNew = new ZeroVector2D
    //this.currentVelocity = boundVelocity(this.currentVelocity + this.velocityIncrement)
    this.currentVelocityNew = boundVelocity(this.currentVelocityNew + velocityIncrementNew)
    this.velocityIncrementNew = new ZeroVector2D
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
  override def toString: String = {
    //this.ID + ", O=" + this.oZone.toString + ", pos=" + this.currentPosition.toString + ", previous pos= " + this._historyPosition.map(_._2) +", D=" + this.dZone.toString + ", vel=" + this.currentVelocity
    this.ID
  }

  /** Prints all the history of the pedestrian in the VisioSafe format. This way, any code which works with VS also
    * works on the results from the simulations.
    *
    * @param refDate year, month and day to print
    * @return one row per position in the historyPosition
    */
  def toVisioSafeFormat(refDate: String = "2013,1,1"): String = {
    def innerPrint(hist: Vector[(NewTime, NewBetterPosition2D)], str: String): String = {
      if (hist.isEmpty) str
      else if (str.isEmpty) innerPrint(hist.tail, refDate + "," + hist.head._1.asVisioSafe + ",0," + round(hist.head._2.X * 1000) + "," + round(hist.head._2.Y * 1000) + "," + this.ID.hashCode)
      else innerPrint(hist.tail, str + "\n" + refDate + "," + hist.head._1.asVisioSafe + ",0," + round(hist.head._2.X * 1000) + "," + round(hist.head._2.Y * 1000) + "," + this.ID.hashCode)
    }
    innerPrint(this._historyPosition, "")
  }
}




