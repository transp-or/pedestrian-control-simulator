package hubmodel.DES

/**
  * Created by nicholas on 5/12/17.
  */

import java.io.{BufferedWriter, File, FileWriter}
import java.util.concurrent.ThreadLocalRandom

import ch.qos.logback.classic.{Level, Logger}
import hubmodel.TimeNumeric.mkOrderingOps
import hubmodel.ped.PedestrianTrait
import hubmodel.tools.Log
import hubmodel.{Time, distance}
import myscala.math.algo.MTree
import myscala.math.vector.{Vector2D, norm}

import scala.collection.immutable.HashMap
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
  *
  */
abstract class PedestrianDES[T <: PedestrianTrait](val startTime: Time,
                                                   val finalTime: Time) {

  /** Randomly generated string to make unique logs */
  val ID: String = Random.alphanumeric take 10 mkString ""

  /** Log for keeping track of events */
  val eventLogger: Logger = new Log("log-DES-events" + ID, Level.TRACE).logger

  /** Log for storing errors */
  val errorLogger: Logger = new Log("log-DES-errors" + ID, Level.TRACE).logger

  /** current time of the simulation */
  private var _currentTime: Time = startTime

  /** getter method for the current time */
  def currentTime: Time = _currentTime

  /** Exit code of the simulation:
    * - defined as -1 on creation
    * - 0 for normal completion
    * - 1 if the simulation wsa aborted if the queues at gates gets too long
    */
  private var _exitCode: Int = -1

  /** getter method for the exit code */
  def exitCode: Int = this._exitCode

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
      //else if (this.action.priority > that.action.priority) 1 // if times are equal, then sort by Action priority. this has higher priority.
      //else if (this.action.priority < that.action.priority) -1 // that has high priority.
      else 0
    }

    override def toString: String = action.toString + " @ " + t
  }

  /** Ordering defined for the Event case class.
    * The ordering is defined by increasing time.
    */
  /*object Event {
    implicit def orderingByTime: Ordering[Event] = Ordering.by(e => -e.t)
  }*/

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
  def insertEventWithDelay[U <: Action](delay: Time)(action: U): Unit = {
    if ((this.currentTime + delay) <= finalTime) eventList += new MyEvent(this.currentTime + delay, action)
  }

  /** Inserts an event into the eventList after a given delay. No need for sorting as the PriorityQueue is always
    * kept in sorted order. Events are only inserted if the ([[currentTime]] + delay) is
    * lower than the [[finalTime]] of the simulation.
    *
    * @param delay  time after the [[currentTime]] at which the event must take place
    * @param action the [[Action]] which must take place
    */
  def insertEventWithDelayNew[U <: Action](delay: Time)(action: U): Unit = {
    if (this.currentTime + delay <= finalTime) eventList += new MyEvent(this.currentTime + delay, action)
  }

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
    if (startTime <= t && t <= finalTime) eventList += new MyEvent(t, action)
  }


  /** immutable structure for storing the list of pedestrians. This object will be updated by the MovePedestrian
    * action and the ComputeRoute action. The only operations which are allowed are:
    *  - adding a new pedestrian to the system
    *  - remove a subset of the population
    */
  /*private var _population: Vector[T] = Vector()

  def append2Population(p: T): Unit = synchronized(_population :+= p)

  def removeFromPopulation(condition: T => Boolean): Unit = {
    synchronized(_population = _population.filterNot(condition))
  }

  def population: Vector[T] = synchronized(_population)*/

  /** New population structure using a map where keys are IDs
    * This makes the usage of a tree for serching neigbors easier */
  private val _populationNew: collection.mutable.Map[String, T] = collection.mutable.Map()

  def insertInPopulation(p: T): Unit = {
    _populationNew += (p.ID -> p)
    this.populationMTree.insert(p.ID, p.currentPosition)
    this.ID2Position = this.ID2Position + (p.ID -> p.currentPosition)
  }

  @deprecated
  def removeFromPopulation(condition: T => Boolean): Unit = _populationNew.retain((k, v) => {
    v.reachedDestination = true
    !condition(v)
  })

  def processCompletedPedestrian(condition: T => Boolean): Unit = {
    val completedPeds: Map[String, T] = this._populationNew.filter(kv => condition(kv._2)).toMap
    completedPeds.values.foreach(_.reachedDestination = true)
    completedPeds.keys.foreach(id => this._populationNew.remove(id))
    this._populationCompleted ++= completedPeds.values
  }

  private var populationMTree: MTree[Vector2D] = new MTree(norm: (Vector2D, Vector2D) => Double)
  private var ID2Position: Map[String, Vector2D] = HashMap()

  def rebuildMTree(): Unit = {
    this.populationMTree = new MTree[Vector2D](distance: (Vector2D, Vector2D) => Double)
    ID2Position = this._populationNew.mapValues(p => (p.ID, p.currentPosition)).values.toMap
    this.populationMTree.insertAll(ID2Position)
  }

  def findNeighbours(id: String, r: Double): Iterable[T] = {
    this.populationMTree.findInRange(id, this.ID2Position(id), r).filterNot(_ == id).map(id => this._populationNew.getOrElse(id, throw new IndexOutOfBoundsException(id))).filterNot(_.reachedDestination)
  }

  def population: Iterable[T] = this._populationNew.values

  def writePopulationTrajectories(file: String): Unit = {
    val f = new File(file)
    val bw = new BufferedWriter(new FileWriter(f))
    var counter: Int = 0
    val totalPeds: Int = (this.populationCompleted ++ this._populationNew.values).size
    for (p <- this.populationCompleted ++ this._populationNew.values) {
      counter += 1
      if (counter % 1 == 0) {
        System.out.print("\r" + counter + "/" + totalPeds + " pedestrians processed")
      }
      bw.write(p.toVisioSafeFormat().stripLineEnd)
      bw.write("\n")
    }
    bw.close()
  }


  /** Immutable structure for storing the pedestrians who have left the system.
    * The only operation which is allowed is to append another population.
    */
  private var _populationCompleted: Vector[T] = Vector()

  @deprecated
  def concatenate2PopulationCompleted(pop: Iterable[T]): Unit = {
    synchronized(_populationCompleted ++= pop)
  }

  def populationCompleted: Vector[T] = synchronized(_populationCompleted)

  // Guo 2011 potential route choice to be continueed. See debug main for code.
  /*def isInsideSpace: Position => Boolean = p => infraSF.polygon.contains(p(0), p(1))

  private val xMin: Double = infraSF.walls.map(w => min(w.startPoint(0), w.endPoint(0))).min
  private val xMax: Double = infraSF.walls.map(w => max(w.startPoint(0), w.endPoint(0))).max
  private val yMin: Double = infraSF.walls.map(w => min(w.startPoint(1), w.endPoint(1))).min
  private val yMax: Double = infraSF.walls.map(w => max(w.startPoint(1), w.endPoint(1))).max
  private val radius: Double = 1.5

  val cellDiscretization: IndexedSeq[MyCell] = (for (
    x <- xMin to xMax by 2*radius*cos(30.0*math.Pi/180.0);
    y <- yMin to yMax by 3*radius)
    yield {
      MyCell(DenseVector(x,y), radius)
    }).filter(h => h.angles.exists(isInsideSpace)) ++ (for (
    x <- (xMin+radius*cos(30.0*math.Pi/180.0)) to xMax by 2*radius*cos(30.0*math.Pi/180.0);
    y <- yMin+1.5*radius to yMax by 3*radius)
    yield {
      MyCell(DenseVector(x,y), radius)
    }).filter(h => h.angles.exists(isInsideSpace))

  val cellConnections: Map[MyCell, List[MyCell]] = cellDiscretization.map(h => h -> cellDiscretization.filter(hin => breeze.linalg.norm(h.center - hin.center) < 1.01*2*radius*cos(30.0*math.Pi/180.0)).filterNot(h == _).toList).toMap
  val doorwayPoints = Vector(9.0 to 11.0 by 0.25).map(y => DenseVector(0.0,y))
*/


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
      //_randU = for (i <- 0 to 10000) yield r.nextDouble
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

  /**
    * First event to be called by the run method from [[PedestrianDES]]
    * This event is simulation dependent, hence it must be overriden in the implementations of the DES.
    * In order to give access to the required data to each Action, the simulation itself is passed
    * as an argument. This makes it very general.
    */
  abstract class GenericStartSim(sim: PedestrianDES[T]) extends Action

  /** Abstract run which must be overriden in implementation.
    * This will call the [[genericRun()]] method and pass the first start as an argument.
    */
  def run(): Unit

  /** Method to run the simulation. This should be overridden in subclasses
    * as specificities are likely to occur. The method should simply call the
    * [[GenericStartSim]] action with a delay of 0.
    *
    * Once this is done, it should run through the [[eventList]] and execute all events
    * until the list is empty.
    */
  def genericRun(startEvent: GenericStartSim): Unit = {
    println("Running simulation " + this.ID + "...")
    insertEventWithDelayNew(new Time(0.0)) {
      startEvent
    }
    while (this.eventList.nonEmpty && this._exitCode == -1) {
      val event = eventList.dequeue()
      this._currentTime = event.t
      /*if (this._currentTime.value % 120.0 == 0) {
        print(" * simulation at " + this._currentTime + " sec\r")
      }*/
      event.action.execute()
    }
    if (this._exitCode == -1) {
      this._exitCode = 0
      println("simulation " + this.ID + " successfully completed !")
    } else {
      println("simulation " + this.ID + "was terminated with exit code " + this._exitCode)
    }
  }

  def abort(code: Int): Unit = {
    this._exitCode = code
  }
}