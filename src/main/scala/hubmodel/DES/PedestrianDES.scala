package hubmodel.DES

/**
  * Created by nicholas on 5/12/17.
  */

import java.util.concurrent.ThreadLocalRandom

import ch.qos.logback.classic.Level
import com.typesafe.scalalogging.{LazyLogging, Logger}
import hubmodel.Position
import hubmodel.TimeNumeric.mkOrderingOps
import hubmodel.ped.{PedestrianNOMAD, PedestrianTrait}
import hubmodel.tools.Log
import hubmodel.{Time, distance}
import myscala.math.algo.MTree
import myscala.math.vector.{Vector2D, norm}

import scala.collection.immutable.HashMap
import scala.reflect.ClassTag
import scala.util.Random

/**
  * Main DES simulator. All the basic information for performing discrete event simulations
  * is defined here. This is one of the parent classes of the pedestrian simulations which
  * will be performed later.
  *
  * The main components for running a discrete event simulator are found here. The specific data and methods are
  * implemented in the subclass.
  *
  * @param startTime start time of the simulation
  * @param finalTime end time of the simulation
  * @tparam T Type of the pedestrian class to use in the simulation
  *
  */
abstract class PedestrianDES[T <: PedestrianNOMAD](val startTime: Time,
                                                   val finalTime: Time) extends LazyLogging {

  ///////////////////////////////////////////////////////////////////////////////////////////////////
  ////////////////////////////// General parameters /////////////////////////////////////////////////
  ///////////////////////////////////////////////////////////////////////////////////////////////////

  /** Randomly generated string to make unique logs */
  val ID: String = Random.alphanumeric take 10 mkString ""


  /** current time of the simulation */
  private var _currentTime: Time = startTime

  /** getter method for the current time */
  def currentTime: Time = _currentTime

  ///////////////////////////////////////////////////////////////////////////////////////////////////
  ////////////////////////////// Logging ////////////////////////////////////////////////////////////
  ///////////////////////////////////////////////////////////////////////////////////////////////////

  // Loggers for the execution of the events and the errors/warnings.
  /*private val loggerPath: Option[String] = if (logPath.isDefined && logPath.get.last == '/') {
    logPath
  } else if (logPath.isDefined && logPath.get.last != '/') {
    Some(logPath.get + "/")
  } else {
    None
  }*/

  /** Log for keeping track of events */
  val eventLogger: Logger = logger

  ///////////////////////////////////////////////////////////////////////////////////////////////////
  ////////////////////////////// Action definition and manipulaiton /////////////////////////////////
  ///////////////////////////////////////////////////////////////////////////////////////////////////

  /** An Action becomes an Event when a time is associated.
    *
    * @param t      time at which the action must be performed
    * @param action the action itself (class with execute method)
    */
  class MyEvent(val t: Time, val action: Action) extends Ordered[MyEvent] {

    // return 0 if the same, negative if this < that, positive if this > that (in terms of priority)
    override def compare(that: MyEvent): Int = {
      if (this.t > that.t) -1 // this.t is larger than that.t, that should be executed before this, hence that has higher priority
      else if (this.t < that.t) 1 // this.t is smaller than that.t, this should be executed before that, hence this has higher priority
      else 0
    }

    /**
      * Writes the event as string with the execution time.
      *
      * @return string with the name of the event and the execution time.
      */
    override def toString: String = action.toString + " @ " + t
  }

  /**
    * Event list which is a PriorityQueue. When inserting a new Event,
    * it is automatically inserted in the correct place. The PriorityQueue is always
    * sorted according to the oredering available for the inner type.
    *
    * It is private as only the [[insertEventWithDelay]] method is allowed to add events.
    */
  protected val eventList: collection.mutable.PriorityQueue[MyEvent] = collection.mutable.PriorityQueue()

  /** Use the new version of this method which uses the [[Time]] class.
    * Inserts an event into the eventList after a given delay. No need for sorting as the PriorityQueue is always
    * kept in sorted order. Events are only inserted if the ([[currentTime]] + delay) is
    * lower than the [[finalTime]] of the simulation.
    *
    * @param delay  time after the [[currentTime]] at which the event must take place
    * @param action the [[Action]] which must take place
    */
  @deprecated
  def insertEventWithDelayOld[U <: Action](delay: Time)(action: U): Unit = {
    if ((this.currentTime + delay) <= finalTime) eventList += new MyEvent(this.currentTime + delay, action)
  }

  /** Inserts an event into the eventList after a given delay. No need for sorting as the PriorityQueue is always
    * kept in sorted order. Events are only inserted if the ([[currentTime]] + delay) is
    * lower than the [[finalTime]] of the simulation.
    *
    * @param delay  time after the [[currentTime]] at which the event must take place
    * @param action the [[Action]] which must take place
    */
  def insertEventWithDelay[U <: Action](delay: Time)(action: U): Unit = {
    if (this.currentTime + delay <= finalTime) eventList += new MyEvent(this.currentTime + delay, action)
  }

  /** Inserts an event witha zero delaay in time. The action will be executed at the current time of the simulator.
    *
    * @param action Child of the  [[Action]] class to inlcude in the event list
    * @tparam U [[Action]]
    */
  def insertEventWithZeroDelay[U <: Action](action: U): Unit = {
    eventList += new MyEvent(this.currentTime, action)
  }


  /** Inserts an event into the eventList at a specific time. No need for sorting as the PriorityQueue is always
    * kept in sorted order. Events are only inserted if the ([[currentTime]] + delay) is
    * lower than the [[finalTime]] of the simulation.
    *
    * @param t      time after the [[currentTime]] at which the event must take place
    * @param action the [[Action]] which must take place
    */
  def insertEventAtAbsolute[U <: Action](t: Time)(action: U): Unit = {
    if (startTime <= t && t < finalTime) eventList += new MyEvent(t, action)
  }

  ///////////////////////////////////////////////////////////////////////////////////////////////////
  ////////////////////////////// Population definition and manipulation /////////////////////////////
  ///////////////////////////////////////////////////////////////////////////////////////////////////


  // New population structure using a map where keys are IDs. This makes the usage of a tree for searching neighbours easier
  private val _populationNew: collection.mutable.Map[String, T] = collection.mutable.Map()

  /**
    * Gets the collection of pedestrians inside the simulation
    *
    * @return
    */
  def population: Iterable[T] = this._populationNew.values

  // Immutable structure for storing the pedestrians who have left the system.
  // The only operation which is allowed is to append another population.
  private var _populationCompleted: Vector[T] = Vector()

  /**
    * Gets the collection of completed pedestrians.
    *
    * @return
    */
  def populationCompleted: Vector[T] = synchronized(_populationCompleted)

  /**
    * Inserts a pedestrian into the simulation.
    *
    * @param p pedestrian to insert
    */
  def insertInPopulation(p: T): Unit = {
    _populationNew += (p.ID -> p)
    this.populationMTree.insert(p.ID, p.currentPosition)
    this.ID2Position = this.ID2Position + (p.ID -> p.currentPosition)
  }

  /**
    * Deal with pedestrians who have reached the final destination.
    *
    * @param condition criteria for determining whether a pedestrian has reached his final destination
    */
  def processCompletedPedestrian(condition: T => Boolean): Unit = {
    val completedPeds: Map[String, T] = this._populationNew.filter(kv => condition(kv._2)).toMap
    completedPeds.values.foreach(p => {
      p.updatePositionHistory(this.currentTime)
      p.reachedDestination = true
      p.exitTime = this.currentTime
    })
    completedPeds.keys.foreach(id => this._populationNew.remove(id))
    this._populationCompleted ++= completedPeds.values
  }

  // Quad-tree for searching for neighboring pedestrians.
  private var populationMTree: MTree[Position] = new MTree(norm: (Position, Position) => Double)

  // Mapping from a peestrian ID to a position
  private var ID2Position: Map[String, Position] = HashMap()

  /**
    * Function which rebuilds the tree with the current positions of the pedestrians
    */
  def rebuildMTree(): Unit = {
    this.populationMTree = new MTree[Vector2D](distance: (Vector2D, Vector2D) => Double)
    ID2Position = this._populationNew.mapValues(p => (p.ID, p.currentPosition)).values.toMap
    this.populationMTree.insertAll(ID2Position)
  }


  /**
    * Find the neighboours within a given radius to the current pedestrian.
    *
    * @param id id of the pedestrian to whom one wished to search
    * @param r  raidus
    * @return collection of pedestrian whithin this range
    */
  def findNeighbours(id: String, r: Double): Iterable[T] = {
    this.populationMTree.findInRange(id, this.ID2Position(id), r).filterNot(_ == id).map(id => this._populationNew.getOrElse(id, throw new IndexOutOfBoundsException(id))).filterNot(_.reachedDestination)
  }

  ///////////////////////////////////////////////////////////////////////////////////////////////////
  ////////////////////////////// Random number generation ///////////////////////////////////////////
  ///////////////////////////////////////////////////////////////////////////////////////////////////

  /**
    * List of uniformly sampled numbers
    */
  private var _randU: IndexedSeq[Double] = for (i <- 0 to 10000) yield ThreadLocalRandom.current.nextDouble(1.0)
  //private var _randU: IndexedSeq[Double] = for (i <- 0 to 10000) yield r.nextDouble

  /** Gets the head of [[_randU]]. If the _randU seq is empty, it will repopulate it
    * and then return the first value.
    *
    * @return first value of randU
    */
  def randU(): Double = {
    if (_randU.isEmpty) {
      _randU = for (i <- 0 to 10000) yield ThreadLocalRandom.current.nextDouble(1.0)
      val res = _randU.head
      _randU = _randU.tail
      res
    }
    else {
      val res = _randU.head
      _randU = _randU.tail
      res
    }
  }

  ///////////////////////////////////////////////////////////////////////////////////////////////////
  ////////////////////////////// Initialization & termination of the simulation /////////////////////
  ///////////////////////////////////////////////////////////////////////////////////////////////////

  /**
    * First event to be called by the run method from [[PedestrianDES]]
    * This event is simulation dependent, hence it must be overridden in the implementations of the DES.
    * In order to give access to the required data to each Action, the simulation itself is passed
    * as an argument. This makes it very general.
    */
  abstract class GenericStartSim(sim: PedestrianDES[T]) extends Action

  /** Abstract run which must be overriden in implementation.
    * This will call the [[genericRun()]] method and pass the first start as an argument.
    */
  def run(): Unit

  /** Exit code of the simulation:
    * - defined as -1 on creation
    * - 0 for normal completion
    * - 1 if the simulation wsa aborted if the queues at gates gets too long
    */
  private var _exitCode: Int = -1

  /** getter method for the exit code */
  def exitCode: Int = this._exitCode

  /** Method to run the simulation. This should be overridden in subclasses
    * as specificities are likely to occur. The method should simply call the
    * [[GenericStartSim]] action with a delay of 0.
    *
    * Once this is done, it should run through the [[eventList]] and execute all events
    * until the list is empty.
    */
  def genericRun(startEvent: GenericStartSim): Unit = {
    println("Running simulation " + this.ID + "...")
    insertEventWithDelay(new Time(0.0)) {
      startEvent
    }
    while (this.eventList.nonEmpty && this._exitCode == -1) {
      val event = eventList.dequeue()
      this._currentTime = event.t
      if (this._currentTime.value % 5.0 == 0) {
        print(" * simulation at " + this._currentTime + " sec\r")
      }
      event.action.execute()
    }
    if (this._exitCode == -1) {
      this._exitCode = 0
      println("simulation " + this.ID + " successfully completed !")
    } else {
      println("simulation " + this.ID + "was terminated with exit code " + this._exitCode)
    }
  }

  /**
    * Aborts the simulation with the exit passed as argument.
    *
    * @param code exit code
    */
  def abort(code: Int): Unit = {
    this._exitCode = code
  }


}