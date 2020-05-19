package hubmodel.DES

/**
  * Created by nicholas on 5/12/17.
  */

import java.util.concurrent.ThreadLocalRandom

import hubmodel.ped.PedestrianNOMAD
import hubmodel.{Position, StrictLogging, distance}
import myscala.math.algo.MTree
import myscala.math.vector.{Vector2D, norm}
import tools.Time
import tools.TimeNumeric.mkOrderingOps

import scala.collection.immutable.HashMap
import scala.util.{Random, Try}

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
abstract class PedestrianDES(val startTime: Time,
                             val finalTime: Time) extends StrictLogging {

  ///////////////////////////////////////////////////////////////////////////////////////////////////
  ////////////////////////////// General parameters /////////////////////////////////////////////////
  ///////////////////////////////////////////////////////////////////////////////////////////////////

  /** Randomly generated string to make unique logs */
  val ID: String = Random.alphanumeric take 10 mkString ""


  /** current time of the simulation */
  private var _currentTime: Time = startTime

  /** getter method for the current time */
  def currentTime: Time = _currentTime

  /** container for keeping the prediction results */
  /*private var _prediction: Option[StatePrediction] = None

  def prediction: Option[StatePrediction] = this._prediction

  def updatePrediction(statePrediction: StatePrediction): Unit = {this._prediction = Some(statePrediction)}

  val isPrediction: Boolean*/

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
  //val eventLogger: Logger = logger

  ///////////////////////////////////////////////////////////////////////////////////////////////////
  ////////////////////////////// Action definition and manipulaiton /////////////////////////////////
  ///////////////////////////////////////////////////////////////////////////////////////////////////


  /**
    * Event list which is a PriorityQueue. When inserting a new Event,
    * it is automatically inserted in the correct place. The PriorityQueue is always
    * sorted according to the oredering available for the inner type.
    *
    * It is private as only the [[insertEventWithDelay]] method is allowed to add events.
    */
  protected val eventList: collection.mutable.PriorityQueue[MyEvent] = collection.mutable.PriorityQueue()

  /** Inserts an event into the eventList after a given delay. No need for sorting as the PriorityQueue is always
    * kept in sorted order. Events are only inserted if the ([[currentTime]] + delay) is
    * lower than the [[finalTime]] of the simulation.
    *
    * @param delay  time after the [[currentTime]] at which the event must take place
    * @param action the [[Action]] which must take place
    */
  def insertEventWithDelay[U <: Action](delay: Time)(action: U): Option[MyEvent] = {
    if (this.currentTime + delay < finalTime) {
      val event = new MyEvent(this.currentTime + delay, action)
      eventList += event
      Some(event)
    } else {
      None
    }
  }

  /** Inserts an event witha zero delaay in time. The action will be executed at the current time of the simulator.
    *
    * @param action Child of the  [[Action]] class to inlcude in the event list
    * @tparam U [[Action]]
    */
  def insertEventWithZeroDelay[U <: Action](action: U): Option[MyEvent] = {
    if (this.currentTime < this.finalTime) {
      val event = new MyEvent(this.currentTime, action)
      eventList += event
      Some(event)
    } else {
      None
    }
  }


  /** Inserts an event into the eventList at a specific time. No need for sorting as the PriorityQueue is always
    * kept in sorted order. Events are only inserted if the ([[currentTime]] + delay) is
    * lower than the [[finalTime]] of the simulation.
    *
    * @param t      time after the [[currentTime]] at which the event must take place
    * @param action the [[Action]] which must take place
    */
  def insertEventAtAbsolute[U <: Action](t: Time)(action: U): Option[MyEvent] = {
    if (startTime <= t && t < finalTime) {
      val event = new MyEvent(t, action)
      eventList += event
      Some(event)
    } else {
      None
    }
  }


  def cloneEventQueueInto(simulator: PedestrianPrediction): Unit = {

    this.eventList.collect {
      case e if e.action.deepCopy(simulator).isDefined && e.t < simulator.finalTime => {
        simulator.eventList.enqueue(new MyEvent(e.t, e.action.deepCopy(simulator).get))
      }
    }
  }



  ///////////////////////////////////////////////////////////////////////////////////////////////////
  ////////////////////////////// Population definition and manipulation /////////////////////////////
  ///////////////////////////////////////////////////////////////////////////////////////////////////


  // New population structure using a map where keys are IDs. This makes the usage of a tree for searching neighbours easier
  private val _populationNew: collection.mutable.Map[String, PedestrianNOMAD] = collection.mutable.Map()

  def pedByID(id: String): Option[PedestrianNOMAD] = {
    if (!this._populationNew.contains(id)) {
      println("error ! ")
    }

    this._populationNew.get(id) match {
      case None => {
        this.errorLogger.error("Pedestrian not in population ! ID=" + id + ". Probably because she missed the exit zone of the AMW.")
        this._exitCode = 2
        None
      }
      case Some(p) => Some(p)
    }
  }

  /**
    * Gets the collection of pedestrians inside the simulation
    *
    * @return
    */
  def population: Iterable[PedestrianNOMAD] = this._populationNew.values

  // Immutable structure for storing the pedestrians who have left the system.
  // The only operation which is allowed is to append another population.
  private var _populationCompleted: Vector[PedestrianNOMAD] = Vector()

  /**
    * Gets the collection of completed pedestrians.
    *
    * @return
    */
  def populationCompleted: Vector[PedestrianNOMAD] = _populationCompleted

  /**
    * Inserts a pedestrian into the simulation.
    *
    * @param p pedestrian to insert
    */
  def insertInPopulation(p: PedestrianNOMAD): Unit = {
    _populationNew += (p.ID -> p)
    this.populationMTree.insert(p.ID, p.currentPosition)
    this.ID2Position = this.ID2Position + (p.ID -> p.currentPosition)
  }

  /**
    * Deal with pedestrians who have reached the final destination.
    *
    * @param condition criteria for determining whether a pedestrian has reached his final destination
    */
  def processCompletedPedestrian(condition: PedestrianNOMAD => Boolean): Unit = {
    val completedPeds: Map[String, PedestrianNOMAD] = this._populationNew.filter(kv => condition(kv._2)).toMap
    completedPeds.values.foreach(p => {
      p.appendAccomplishedRoute(this.currentTime, p.finalDestination, p.currentPosition)
      p.updatePositionHistory(this.currentTime, scala.math.max(p.isolationTypePed, p.isolationTypeObs))
      p.reachedDestination = true
      p.exitTime = this.currentTime
    })
    completedPeds.keys.foreach(id => this._populationNew.remove(id))
    this._populationCompleted ++= completedPeds.values
  }

  // Quad-tree for searching for neighboring pedestrians.
  private var populationMTree: MTree[Position] = new MTree(norm: (Position, Position) => Double, 30)

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
    * @param id id of the pedestrian to whom one wishes to search
    * @param r  radius
    * @return collection of pedestrian whithin this range
    */
  def findNeighbours(id: String, r: Double): Iterable[PedestrianNOMAD] = {
    this.populationMTree
      .findInRange(id, this.ID2Position(id), r)
      .filterNot(p => p == id || !this._populationNew.keySet.contains(p))
      .map(id => this._populationNew.getOrElse(id, throw new IndexOutOfBoundsException(id)))
      .filterNot(_.reachedDestination)
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


  /** Parent of the state evaluation methods used in the main simulation and the prediction simulations.
    * This action has priority over other actions at equal execution time, hence priority set to 1.
    *
    */
  trait StateEvaluationActionDES extends Action {
    override val priority: Int = 100
  }

  def insertStateEvaluationStart(stateEval: StateEvaluationActionDES): Unit = {
    this.insertEventWithZeroDelay(stateEval)
  }


  val simulationType: String = "Generic"

  val verbose: Boolean = true

  /**
    * First event to be called by the run method from [[PedestrianDES]]
    * This event is simulation dependent, hence it must be overridden in the implementations of the DES.
    * In order to give access to the required data to each Action, the simulation itself is passed
    * as an argument. This makes it very general.
    */
  abstract class GenericStartSim(sim: PedestrianDES) extends Action

  abstract class GenericFinishSim(sim: PedestrianDES, val finalTime: Time) extends Action

  trait LogStateDES extends Action


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
  def genericRun(startEvent: GenericStartSim, endEvent: GenericFinishSim): Unit = {
    if (verbose) {    this.logger.info("Starting simulation (" + this.simulationType + ") " + this.ID + " @" + this.currentTime) }

    this.insertEventWithZeroDelay{startEvent}
    this.eventList.addOne(new MyEvent(endEvent.finalTime, endEvent))

    while (this.eventList.nonEmpty && this._exitCode == -1) {
      /*if (this.currentTime.value.toDouble > 625.0 && this.simulationType == "main simulation"){
        println("debug")
      }*/
      val event = eventList.dequeue()
      this._currentTime = event.t
      if (!event.skip) { event.action.execute()}
    }

    if (this._exitCode == -1) {
      this._exitCode = 0
      if (verbose) {this.logger.info("Completed simulation (" + this.simulationType + ") " + this.ID + " !")}
    } else {
      this.logger.info("Terminated simulation (" + this.simulationType + ") " + this.ID + ". Terminated with exit code " + this._exitCode)
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