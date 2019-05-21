package hubmodel.ped

import java.util.concurrent.ThreadLocalRandom

import hubmodel.tools.Time
import hubmodel.tools.cells.Rectangle
import hubmodel.{Position, pedestrianWalkingSpeed, _}
import myscala.math.vector.ZeroVector2D


/**
  * Created by nicholas on 5/13/17.
  */


/** Pedestrian used for the disaggregate models and the network based route choice model.
  *
  * @param origin             origin zone of the pedestrian
  * @param finalDestination   destination zone of the pedestrian
  * @param speed        free flow speed of the individual
  * @param entryTime          entry time into the system
  * @param currentPosition    current position (general point)
  * @param currentDestination intermediate destination point (next target)
  * @param route              initial route
  */
class PedestrianSim(val origin: Rectangle,
                    val finalDestination: Rectangle,
                    val entryTime: Time,
                    val logFullHistory: Boolean = false) extends PedestrianTrait {

  /** Free flow walking speed of the pedestrian
    *
    */
  val desiredWalkingSpeed: Double = {

    // samples once the speed
    var speed = pedestrianWalkingSpeed

    // resamples if the speed is negative
    while (speed < 0.0) {
      speed = pedestrianWalkingSpeed
    }
    speed
  }

  /** current position of the pedestrian
    *
    */
  var currentPosition: Position = origin.uniformSamplePointInside

  /** Previous position of the pedestrian. This avoids using an list or array if the full trajectory of the pedestrian
    * is not of interest.
    */
  var previousPosition: Position = currentPosition


  // ******************************************************************************************
  //                                PRIVATE MEMBERS
  // ******************************************************************************************

  /** History of the pedestrians positions */
  protected var _historyPosition: Vector[(Time, Position)] = Vector((entryTime, currentPosition))

  var route: List[Rectangle] = List()

  /** intermediate destination of the pedestrian */
  var currentDestination: Position = new Position(0, 0) //route.head.uniformSamplePointInside

  // Checks that the velocity is realistic
  if (desiredWalkingSpeed < 0.0 || desiredWalkingSpeed > 4.0) {throw new IllegalArgumentException("Unacceptable free flow velocity")}

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

  /** collection of zones which must not block the pedestrian (make him wait) */
  val freedFrom: scala.collection.mutable.ArrayBuffer[String] = scala.collection.mutable.ArrayBuffer()

  /** time spent in each monitored area. The first value is the entrance time and the second value the exit time. */
  val timeInMonitoredAreas: scala.collection.mutable.Map[String,(Time, Time)] = scala.collection.mutable.Map()

  /** target zone */
  var nextZone: Rectangle = finalDestination //route.head

  var previousZone: Rectangle = origin //route.head

  /* Position increment to be added at the next time interval */
  var positionIncrement: Position = new ZeroVector2D

  /* Velocity increment to be added at the next time inteval */
  var velocityIncrement: Velocity = new ZeroVector2D


  // ******************************************************************************************
  //                    AGENT-BASED PARAMETERS REQUIRED BY THE MICROSCOPIC MODELS
  // ******************************************************************************************

  val r: Double = ThreadLocalRandom.current.nextDouble(0.2, 0.3)
  val a0: Double = 10
  //ThreadLocalRandom.current.nextDouble(8.0, 10.0)
  val r0: Double = 0.16
  //ThreadLocalRandom.current.nextDouble(0.10, 0.5)
  val a1: Double = 2.0
  //ThreadLocalRandom.current.nextDouble(8.0, 10.0)
  val r1: Double = 0.013 // ThreadLocalRandom.current.nextDouble(0.10, 0.5)
  val k0: Double = 1000
  //ThreadLocalRandom.current.nextDouble(800.0, 1000.0)
  val kappa: Double = 1000.0
  //ThreadLocalRandom.current.nextDouble(800.0, 1000.0)
  val ief: Double = 3.0
  //ThreadLocalRandom.current.nextDouble(0.5, 6.0)
  val ieb: Double = 0.65
  //ThreadLocalRandom.current.nextDouble(0.5, 6.0)
  val c0plus: Double = 0.9
  //ThreadLocalRandom.current.nextDouble(0.7, 1.5)
  val c0min: Double = 0.9 //ThreadLocalRandom.current.nextDouble(0.7, 1.5)


  // ******************************************************************************************
  //                              GETTER - SETTER METHODS
  // ******************************************************************************************

  /** getter method for the history of the positions */
  def getHistoryPosition: Vector[(Time, Position)] = _historyPosition


  /** Adds the current position (currentPosition) to the history */
  def updatePositionHistory(t: Time): Unit = {
    //if (this.currentPosition != this._historyPosition.last._2) {
    this.previousPosition = this.currentPosition
    if (logFullHistory) {
      this._historyPosition = this._historyPosition :+ (t, this.currentPosition)
    }
    //}
  }

  def setCurrentDestination(pos: Position): Unit = {
    this.currentDestination = pos
  }

  // ******************************************************************************************
  //                      MOVES THE PEDESTRIAN TO THE NEW POSITION
  // ******************************************************************************************

  /** Moves each pedestrian with the already compuoted increments
    * The increments are set by the [[hubmodel.mvmtmodels.SocialForceESI]] model.
    */
  def move(): Unit = {
    this.travelDistance += positionIncrement.norm
    this.currentPosition = this.currentPosition + positionIncrement
    this.positionIncrement = new ZeroVector2D
    this.currentVelocity = boundVelocity(this.currentVelocity + velocityIncrement)
    this.velocityIncrement = new ZeroVector2D
  }


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
      else if (str.isEmpty) innerPrint(hist.tail, refDate + "," + hist.head._1.asVisioSafe + ",0," + math.round(hist.head._2.X * 1000) + "," + math.round(hist.head._2.Y * 1000) + "," + this.ID)
      else innerPrint(hist.tail, str + "\n" + refDate + "," + hist.head._1.asVisioSafe + ",0," + math.round(hist.head._2.X * 1000) + "," + math.round(hist.head._2.Y * 1000) + "," + this.ID)
    }

    innerPrint(this._historyPosition, "")
  }


  // ******************************************************************************************
  //                                   ALTERNATIVE CONSTRUCTORS
  // ******************************************************************************************

  /**
    *
    * @param oZone
    * @param dZone
    * @param entryTime
    * @param posO
    * @param logFullHistory
    */
  def this(oZone: Rectangle, dZone: Rectangle, entryTime: Time, posO: Position, logFullHistory: Boolean) {
    this(oZone, dZone, entryTime, logFullHistory) // velocity taken from VS data
    this.currentPosition = posO
  }

  /** Writes the pedestrian to a JSON string.
    *
    * @param completed
    * @return
    */
  def toJSON(completed: Boolean): String = {
    "{" +
    "\"o\":\"" + this.origin + "\"," +
    "\"d\":\"" + this.finalDestination + "\"," +
    "\"tt\":" + this.travelTime + "," +
    "\"entry\":" + this.entryTime + "," +
    "\"exit\":" + {if (completed) this.exitTime else {"null"}} + "," +
    "\"td\":" + this.travelDistance + "," +
    "\"gates\": [" + {if (this.freedFrom.nonEmpty) {"\"" + this.freedFrom.mkString("\",\"") + "\""} else {""}} + "]," +
    "\"tt-monitored-zones\": [" + {if(this.timeInMonitoredAreas.nonEmpty) {"{" + this.timeInMonitoredAreas.map(kv => "\"mz_id\": \"" + kv._1 + "\"," + "\"tt\":" + (kv._2._2-kv._2._1).toString).mkString("},{") + "}"} else {""} } + "]"+
    "}"
  }
}




