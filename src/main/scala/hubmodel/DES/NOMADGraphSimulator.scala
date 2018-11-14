package hubmodel.DES

import hubmodel._
import hubmodel.demand.{PTInducedQueue, PedestrianFlowFunction_New, PedestrianFlowPT_New, PedestrianFlow_New, PublicTransportSchedule}
import hubmodel.mgmt.{ControlDevices, EvaluateState}
import hubmodel.mvmtmodels.NOMAD.NOMADIntegrated
import hubmodel.mvmtmodels.RebuildTree
import hubmodel.ped.{PedestrianNOMAD, PedestrianNOMADWithGraph, PedestrianSim}
import hubmodel.route.UpdateRoutes
import hubmodel.supply.NodeParent
import hubmodel.supply.continuous.{ContinuousSpace, Wall}
import hubmodel.supply.graph.{RouteGraph, RouteGraphMultiple, RouteGraphParent, StartFlowGates}
import hubmodel.tools.cells.{DensityMeasuredArea, Rectangle, isInVertex}

class NOMADGraphSimulator[T <: PedestrianNOMAD](st: Time,
                          et: Time,
                          logDir: Option[String],
                          val sf_dt: Time,
                          val route_dt: Time,
                          val evaluate_dt: Time,
                          val rebuildTreeInterval: Option[Time],
                          val spaceMicro: ContinuousSpace,
                          val graph: RouteGraphParent[T],
                          val timeTable: PublicTransportSchedule,
                          val stop2Vertices: NodeParent => Iterable[Rectangle],
                          val controlDevices: ControlDevices) extends PedestrianDES[T](st, et, logDir) {

  /**
    * Access for the wall collection which is mostly contained in the SF infrastructrue file but some movable walls
    * are found in the [[ControlDevices]] infrastructure file.
    * @return collection of [[Wall]] to interact with pedestrians.
    */
  def walls: Iterable[Wall] = spaceMicro.walls ++ controlDevices.flowSeparators.map(_.getWall)

  /* checks whether a pedestrian has reach is next destination zone */
  def intermediateDestinationReached: PedestrianSim => Boolean = p => isInVertex(p.nextZone)(p.currentPosition)

  /* Updates the next destination */
  val updateIntermediateDestination: T => Unit = ped => graph match {
    case rs: RouteGraph[T] => { rs.processIntermediateArrival(ped) }
    case rm: RouteGraphMultiple[T] => { rm.processIntermediateArrival(ped) }
  }

  /* Computes the first route  */
  val setFirstRoute: T => Unit = ped => graph match {
    case rs: RouteGraph[T] => { rs.processIntermediateArrival(ped) }
    case rm: RouteGraphMultiple[T] => { rm.setRouteFirst(ped) }
  }

  /* checks if the pedestrian has reached is final destination */
  def finalDestinationReached: PedestrianSim => Boolean = p => isInVertex(p.finalDestination)(p.currentPosition)

  println("Simulator configuration:")

  /** Indicator wether the density should be measured */
  val measureDensity: Boolean = controlDevices.monitoredAreas.nonEmpty && controlDevices.amws.isEmpty && controlDevices.binaryGates.isEmpty && controlDevices.flowGates.isEmpty
  if (measureDensity) {
    println(" * measuring density")
  }

  /** Indicator whether flow gates are present */
  val useFlowGates: Boolean = controlDevices.flowGates.nonEmpty
  if (useFlowGates) {
    println(" * using flow gates")
  }

  /** Indicator whether binary gaets are present */
  val useBinaryGates: Boolean = controlDevices.binaryGates.nonEmpty
  if (useBinaryGates) {
    println(" * using binary gates")
  }

    /** Indicator whether an m-tree is used to perform neighbour search */
  val useTreeForNeighbourSearch: Boolean = rebuildTreeInterval.isDefined
  if (useTreeForNeighbourSearch) {
    println(" * using m-tree for neighbour search")
  }

  /** Indicator whether flow separators are used */
  val useFlowSep: Boolean = controlDevices.flowSeparators.nonEmpty
  if (useFlowSep) {
    println(" * using flow separators")
  }

  // Zones where some KPI should be computed. They must be inititialized before they can be used.
  val criticalAreas: Map[String, DensityMeasuredArea] = controlDevices.monitoredAreas.map(zone => zone.name -> zone).toMap
  criticalAreas.values.foreach(_.initializeContainers(this.startTime))

  val gatesHistory: collection.mutable.ArrayBuffer[(Time, List[(String, Boolean)])] = collection.mutable.ArrayBuffer()

  val PTInducedFlows: collection.mutable.Map[Rectangle, PTInducedQueue[T]] = collection.mutable.Map()

  val ODZones: Iterable[Rectangle] = this.graph.vertexMap.values.filter(_.isOD)

  var regulatorIntegralAction: Double = 0.0




  /**
    * Class to initialize the simulation. The first calls to reccurent events like the [[NOMADIntegrated]]
    * and the [[EvaluateState]] are made.
    * @param sim simulation object
    */
  class StartSim(sim: NOMADGraphSimulator[T]) extends super.GenericStartSim(sim) {
    override def execute(): Unit = {
      sim.eventLogger.trace("Simulation components:")

      if (useFlowGates) {
        sim.eventLogger.trace(" * flow gates: " + sim.controlDevices.flowGates.map(_.toString).mkString("\n  * "))
      }

      if (useFlowSep) {
        sim.eventLogger.trace {
          " * flow separators: " + sim.controlDevices.flowSeparators.map(_.toString).mkString("\n  * ")
        }
      }

      sim.eventLogger.trace("sim-time=" + sim.currentTime + ": simulation started. dt=" + sf_dt)

      // Inserts the update routes events in the simulation
      sim.insertEventWithZeroDelay(new UpdateRoutes(sim))

      // Inserts the movement model
      sim.insertEventWithZeroDelay(new NOMADIntegrated(sim))

      // Makes the simulation keep track of the state of the system
      if (sim.measureDensity || sim.useFlowSep || sim.useBinaryGates) sim.insertEventWithZeroDelay(new EvaluateState(sim))

      // Starts the flow gates
      if (sim.useFlowGates) sim.insertEventWithZeroDelay(new StartFlowGates(sim))

      // Uses the quad-tree for searching neighbours
      if (sim.useTreeForNeighbourSearch) sim.insertEventWithDelay(new Time(0.0))(new RebuildTree(sim))

      // Uses the safeguard on pedestrian queues
      sim.insertEventWithZeroDelay(new SafeGuard())
    }
  }

  /**
    * Class to interrupt the simulation if some criteria is met. The idea is to prevent simulations running for
    * ridiculous times if some unfeasible situation has occured.
    */
  private class SafeGuard() extends Action[T] {
    override def execute(): Unit = {
      if (useFlowGates && controlDevices.flowGates.exists(fg => fg.pedestrianQueue.size > 100)) {
        abort(1)
      } else {
        insertEventWithDelay(Time(10))(new SafeGuard())
      }
    }
  }

  /**
    * Runs the simulation. This should be called after the processing events have been inserted.
    */
  override def run(): Unit = super.genericRun(new StartSim(this))


  /**
    * Creates a string with the pedestrians who have exited the simulation as csv
    */
  @deprecated
  def printPopulationCompleted(): Option[String] = {
    if (this.populationCompleted.nonEmpty) Some(this.populationCompleted.tail.foldLeft(this.populationCompleted.head.toVisioSafeFormat()) { (s: String, p: PedestrianSim) => s + "\n" + p.toVisioSafeFormat() })
    else None
  }

  /**
    * Collects the parameters used for creating the simulation. The arguments are passed as a tuple.
    * @return
    */
  def getSetupArguments: (
    Time,
      Time,
      Time,
      Time,
      Time,
      Option[Time],
      ContinuousSpace,
      RouteGraphParent[T],
      PublicTransportSchedule,
      NodeParent => Iterable[Rectangle],
      ControlDevices
    ) = (
    startTime,
    finalTime,
    sf_dt,
    route_dt,
    evaluate_dt,
    rebuildTreeInterval,
    spaceMicro,
    graph,
    timeTable,
    stop2Vertices,
    controlDevices
  )

}
