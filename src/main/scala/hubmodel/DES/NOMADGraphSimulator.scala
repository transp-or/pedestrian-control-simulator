package hubmodel.DES

import hubmodel._
import hubmodel.control.{ControlDevices, EvaluateState}
import hubmodel.demand.{PTInducedQueue, PublicTransportSchedule}
import hubmodel.mvmtmodels.NOMAD.NOMADIntegrated
import hubmodel.mvmtmodels.{RebuildPopulationTree, UpdateClosestWall}
import hubmodel.ped.{PedestrianNOMAD, PedestrianSim}
import hubmodel.route.UpdateRoutes
import hubmodel.supply.NodeParent
import hubmodel.supply.continuous.{ContinuousSpace, Wall}
import hubmodel.supply.graph._
import tools.Time
import tools.cells.{DensityMeasuredArea, Vertex, isInVertex}

abstract class NOMADGraphSimulator(st: Time,
                                           et: Time,
                                           val sf_dt: Time,
                                           val route_dt: Time,
                                           val evaluate_dt: Time,
                                           val rebuildTreeInterval: Option[Time],
                                           val spaceMicro: ContinuousSpace,
                                           val graph: GraphContainer,
                                           val timeTable: Option[PublicTransportSchedule],
                                           val stop2Vertex: Stop2Vertex,
                                           val controlDevices: ControlDevices,
                                           val logFullPedestrianHistory: Boolean = false) extends PedestrianDES(st, et) {

  // TODO: continue implementing this
  //val flowSeparators: Option[Vector[FlowSeparator[_, _]]] = controlDevices.flowSepParams.flatMap(fsp => )

  def stop2Vertices: NodeParent => Iterable[Vertex] = mappingConceptualNode2GraphNodes(this.graph)(stop2Vertex.stop2Vertices, if (timeTable.isDefined) {
    timeTable.get.timeTable
  } else {
    Map()
  })

  /**
    * Access for the wall collection which is mostly contained in the SF infrastructrue file but some movable walls
    * are found in the [[ControlDevices]] infrastructure file.
    *
    * @return collection of [[Wall]] to interact with pedestrians.
    */
  def walls: Iterable[Wall] = spaceMicro.walls ++ controlDevices.flowSeparators.map(_.getWall)

  /* checks whether a pedestrian has reach is next destination zone */
  def intermediateDestinationReached: PedestrianSim => Boolean = p => isInVertex(p.nextZone)(p.currentPosition)

  /* Updates the next destination */
  val updateIntermediateDestination: PedestrianNOMAD => Unit = ped => graph.processIntermediateArrival(ped)
  /* => graph match {
      case rm: RouteGraphMultiple[T] => { rm.processIntermediateArrival(ped) }
      case rs: RouteGraph[T] => { rs.processIntermediateArrival(ped) }
    }*/

  /* Computes the first route  */
  val setFirstRoute: PedestrianNOMAD => Unit = ped => graph match {
    case rm: MultipleGraph => {
      rm.setRouteFirst(ped)
    }
    case rs: SingleGraph => {
      rs.processIntermediateArrival(ped)
    }
  }

  /* checks if the pedestrian has reached is final destination */
  def finalDestinationReached: PedestrianSim => Boolean = p => isInVertex(p.finalDestination)(p.currentPosition)

  logger.info("Simulator configuration:")

  /** Indicator wether the density should be measured */
  val measureDensity: Boolean = controlDevices.monitoredAreas.nonEmpty && controlDevices.amws.isEmpty && controlDevices.binaryGates.isEmpty && controlDevices.flowGates.isEmpty
  if (measureDensity) {
    logger.info(" * measuring density")
  }

  /** Indicator whether flow gates are present */
  val useFlowGates: Boolean = controlDevices.flowGates.nonEmpty
  if (useFlowGates) {
    logger.info(" * using flow gates")
  }

  /** Indicator whether binary gaets are present */
  val useBinaryGates: Boolean = controlDevices.binaryGates.nonEmpty
  if (useBinaryGates) {
    logger.info(" * using binary gates")
  }

  /** Indicator whether an m-tree is used to perform neighbour search */
  val useTreeForNeighbourSearch: Boolean = rebuildTreeInterval.isDefined
  if (useTreeForNeighbourSearch) {
    logger.info(" * using m-tree for neighbour search")
  }


  /** Indicator whether flow separators are used */
  val useFlowSep: Boolean = controlDevices.flowSeparators.nonEmpty
  if (useFlowSep && !controlDevices.fixedFlowSeparators) {
    logger.info(" * using dynamic flow separators")
  } else {
    logger.info(" * using static flow separators")
  }

  val useAMWs: Boolean = controlDevices.amws.nonEmpty
  if (useFlowSep) {
    logger.info(" * using accelerated moving walkways")
  }

  // Zones where some KPI should be computed. They must be inititialized before they can be used.
  val criticalAreas: Map[String, DensityMeasuredArea] = controlDevices.monitoredAreas.map(zone => zone.name -> zone).toMap
  criticalAreas.values.foreach(_.initializeContainers(this.startTime))

  val gatesHistory: collection.mutable.ArrayBuffer[(Time, List[(String, Boolean)])] = collection.mutable.ArrayBuffer()

  val PTInducedFlows: collection.mutable.Map[Vertex, PTInducedQueue] = collection.mutable.Map()

  val ODZones: Iterable[Vertex] = this.graph.vertexMapNew.values.filter(_.isOD)

  val transferringPassengers: collection.mutable.Set[String] = collection.mutable.Set()

  /**
    * Class to initialize the simulation. The first calls to reccurent events like the [[NOMADIntegrated]]
    * and the [[EvaluateState]] are made.
    *
    * @param sim simulation object
    */
  class StartSim[U <: P](sim: NOMADGraphSimulator) extends super.GenericStartSim(sim) {
    override def execute(): Unit = {

      if (sim.useFlowGates) {
        sim.logger.info(" * flow gates: " + sim.controlDevices.flowGates.map(_.toString).mkString("\n  * "))
      }

      if (sim.useFlowSep) {
        sim.logger.info {
          " * flow separators: " + sim.controlDevices.flowSeparators.map(_.toString).mkString("\n  * ")
        }
      }

      //sim.logger.info("Starting simulation " + sim.ID + " @" + this.sim.currentTime)

      sim.eventLogger.trace("sim-time=" + sim.currentTime + ": simulation started. dt=" + sim.sf_dt)

      // Inserts the update routes events in the simulation
      sim.insertEventWithZeroDelay(new UpdateRoutes(sim))

      // Inserts the movement model
      sim.insertEventWithZeroDelay(new NOMADIntegrated(sim))

      // Makes the simulation keep track of the state of the system.
      // THIS IS DONE BY THE SIMULATION AND PREDICITON CLASSED SINCE THE STATE EVALUATION IS DEPENDENT ON THE TYPE OF SIM
      /*if (sim.measureDensity || sim.useFlowSep || sim.useBinaryGates || sim.useFlowGates || sim.useAMWs) {
        sim.insertEventWithZeroDelay(new EvaluateState(sim))
      }*/

      // Starts the flow gates
      if (sim.useFlowGates) {
        sim.insertEventWithZeroDelay(new StartFlowGates(sim))
      }

      // Uses the quad-tree for searching neighbours
      if (sim.useTreeForNeighbourSearch) {
        sim.insertEventWithDelay(new Time(0.0))(new RebuildPopulationTree(sim))
      }

      // start the reccurrent update of walls.
      sim.insertEventWithZeroDelay(new UpdateClosestWall(sim))

      // Uses the safeguard on pedestrian queues
      sim.insertEventWithZeroDelay(new SafeGuard())
    }

    type A = StartSim[P]

    type B = NOMADGraphSimulator

override def deepCopy(simulator: PedestrianPrediction): Option[A] = {
      Some(new StartSim(simulator))
    }
  }

  /**
    * Class to interrupt the simulation if some criteria is met. The idea is to prevent simulations running for
    * ridiculous times if some unfeasible situation has occured.
    */
  private class SafeGuard extends Action {
    override def execute(): Unit = {
      if (useFlowGates && controlDevices.flowGates.exists(fg => fg.pedestrianQueue.size > 100)) {
        abort(1)
      } else {
        insertEventWithDelay(Time(10))(new SafeGuard())
      }
    }

    type A = SafeGuard

    type B = NOMADGraphSimulator

override def deepCopy(simulator: PedestrianPrediction): Option[A] = {
      Some(new SafeGuard)
    }
  }

  /**
    * Runs the simulation. This should be called after the processing events have been inserted.
    */
  override def run(): Unit = { super.genericRun(new StartSim(this)) }


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
    *
    * @return
    */
  @deprecated
  def getSetupArguments: SimulatorParameters = (
    startTime,
    finalTime,
    sf_dt,
    route_dt,
    evaluate_dt,
    rebuildTreeInterval,
    spaceMicro,
    graph,
    timeTable,
    this.stop2Vertex,
    controlDevices
  )

  def getSetupArgumentsNew: SimulationParametersClass = {
    new SimulationParametersClass(
      startTime,
      finalTime,
      sf_dt,
      route_dt,
      evaluate_dt,
      rebuildTreeInterval,
      spaceMicro,
      graph,
      timeTable,
      this.stop2Vertex,
      controlDevices,
      logFullPedestrianHistory
    )
  }

}
