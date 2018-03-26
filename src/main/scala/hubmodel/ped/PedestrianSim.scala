package hubmodel.ped

import java.util.concurrent.ThreadLocalRandom

import breeze.numerics.round
import hubmodel._
import hubmodel.tools.cells.RectangularVertexTrait
import myscala.math.vector.ZeroVector2D


/**
  * Created by nicholas on 5/13/17.
  */


/** Pedestrian used for the disaggregate models and the network based route choice model.
  *
  * @param origin              origin zone of the pedestrian
  * @param finalDestination              destination zone of the pedestrian
  * @param freeFlowVel        free flow speed of the individual
  * @param entryTime          entry time into the system
  * @param currentPosition    current position (general point)
  * @param currentDestination intermediate destination point (next target)
  * @param route              initial route
  */
class PedestrianSim(val origin: RectangularVertexTrait,
                    val finalDestination: RectangularVertexTrait,
                    val freeFlowVel: Double,
                    val entryTime: Time,
                    var route: List[RectangularVertexTrait]) extends PedestrianTrait {

  /** current position of the pedestrian */
  var currentPosition: Position = origin.uniformSamplePointInside

  /** intermediate destination of the pedestrian */
  var currentDestination: Position = route.head.uniformSamplePointInside

  // Checks that the velocity is realistic
  assert(freeFlowVel > 0.0, "Unacceptable free flow velocity")

  /** current velocity, initialized to 0.0 */
  var currentVelocity: Velocity = new ZeroVector2D

  /** total travel time */
  var travelTime: Time = Time(0.0)

  /** total travelled distance */
  var travelDistance: Double = 0.0

  /** exit time from the system */
  var exitTime: Time = Time(0.0)

  /** is the pedestrian waiting in a zone */
  var isWaiting: Boolean = false

  /** collection of zones whichi must not block the pedestrian (make him wait) */
  val freedFrom: scala.collection.mutable.ArrayBuffer[String] = scala.collection.mutable.ArrayBuffer()

  /** target zone */
  var nextZone: RectangularVertexTrait = route.head

  /* Position increment to be added at the next time interval */
  var positionIncrement: Position = new ZeroVector2D

  /* Velocity increment to be added at the next time inteval */
  var velocityIncrement: Velocity = new ZeroVector2D


  // ******************************************************************************************
  //                    AGENT-BASED PARAMETERS REQUIRED BY THE MICROSCOPIC MODELS
  // ******************************************************************************************

  val r: Double = ThreadLocalRandom.current.nextDouble(0.2, 0.3)
  val a0: Double = ThreadLocalRandom.current.nextDouble(8.0 , 12.0)
  val r0: Double = ThreadLocalRandom.current.nextDouble(0.10, 0.6 )
  val a1: Double = ThreadLocalRandom.current.nextDouble(8.0 , 12.0)
  val r1: Double = ThreadLocalRandom.current.nextDouble(0.10, 0.6 )
  val k0: Double = ThreadLocalRandom.current.nextDouble(800.0,1200.0)
  val k1: Double = ThreadLocalRandom.current.nextDouble(800.0,1200.0)

  // ******************************************************************************************
  //                              GETTER - SETTER METHODS
  // ******************************************************************************************

  /** getter method for the history of the positions */
  def getHistoryPosition: Vector[(Time, Position)] = _historyPosition

  /** Adds the current position (currentPosition) to the history */
  def addHistory(t: Time): Unit = {
    _historyPosition = _historyPosition :+ (t, this.currentPosition)
  }

  def setCurrentDestination(pos: Position): Unit = {
    this.currentDestination = pos
    this.currentDestination = pos
  }

  // ******************************************************************************************
  //                      MOVES THE PEDESTRIAN TO THE NEW POSITION
  // ******************************************************************************************

  /** Moves each pedestrian with the already compuoted increments
    * The increments are set by the [[hubmodel.mvmtmodels.SocialForceESI]] model.
    */
  def move(): Unit = {
    this.currentPosition = this.currentPosition + positionIncrement
    this.positionIncrement = new ZeroVector2D
    this.currentVelocity = boundVelocity(this.currentVelocity + velocityIncrement)
    this.velocityIncrement = new ZeroVector2D
  }

  // ******************************************************************************************
  //                                PRIVATE MEMBERS
  // ******************************************************************************************

  /** History of the pedestrians positions */
  private var _historyPosition: Vector[(Time, Position)] = Vector((entryTime, currentPosition))


  // ******************************************************************************************
  //                                PRINTING METHODS
  // ******************************************************************************************

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
    def innerPrint(hist: Vector[(Time, Position)], str: String): String = {
      if (hist.isEmpty) str
      else if (str.isEmpty) innerPrint(hist.tail, refDate + "," + hist.head._1.asVisioSafe + ",0," + round(hist.head._2.X * 1000) + "," + round(hist.head._2.Y * 1000) + "," + this.ID.hashCode)
      else innerPrint(hist.tail, str + "\n" + refDate + "," + hist.head._1.asVisioSafe + ",0," + round(hist.head._2.X * 1000) + "," + round(hist.head._2.Y * 1000) + "," + this.ID.hashCode)
    }
    innerPrint(this._historyPosition, "")
  }

  // ******************************************************************************************
  //                                   ALTERNATIVE CONSTRUCTORS
  // ******************************************************************************************

  /**
    * Overloaded constructor where the freeFlowVel is sampled from Weidmann's distribution.
    *
    * @param oZone     origin zone of the pedestrian
    * @param dZone     destination zone of the pedestrian
    * @param entryTime entry time into the system
    * @param posO      current position (general point)
    * @param route     initial route
    */
  def this(oZone: RectangularVertexTrait, dZone: RectangularVertexTrait, entryTime: Time, posO: Position, route: List[RectangularVertexTrait]) {
    this(oZone, dZone, 1.34 + 0.0 * ThreadLocalRandom.current().nextGaussian(), entryTime, route)
    this.currentPosition = posO
  }
}




