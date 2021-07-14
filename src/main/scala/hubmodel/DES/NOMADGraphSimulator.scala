package hubmodel.DES

import hubmodel._
import hubmodel.control.amw.{AMWPolicy, StaticEngineeringSolution}
import hubmodel.control.{ComputePedestrianDensity, ControlDevices, EvaluateState, ReinitializeFlowCounters, UpdateDensityReactiveAMWs}
import hubmodel.demand.{PTInducedQueue, PublicTransportSchedule}
import hubmodel.mvmtmodels.NOMAD.NOMADIntegrated
import hubmodel.mvmtmodels.{RebuildPopulationTree, UpdateClosestWall}
import hubmodel.ped.{PedestrianNOMAD, PedestrianSim}
import hubmodel.route.UpdatePedestrianRoutes
import hubmodel.supply.NodeParent
import hubmodel.supply.continuous.{ContinuousSpace, Wall}
import hubmodel.supply.graph._
import tools.Time
import tools.cells.{DensityMeasuredArea, Vertex, isInVertex}
import tools.exceptions.ControlDevicesException
import tools.TimeNumeric.mkOrderingOps

abstract class NOMADGraphSimulator(params: SimulationInputParameters) extends PedestrianDES(params.startTime, params.endTime) {

  val motionModelUpdateInterval: Time = params.motionModelUpdateInterval
  val updateRoutesInterval: Time = params.updateRoutesInterval
  val stateEvaluationInterval: Time = params.stateEvaluationInterval.get
  val graph: GraphContainer = params.graph
  val timeTable: Option[PublicTransportSchedule] = params.timeTable
  val spaceMicro: ContinuousSpace = params.spaceMicro
  val controlDevices: ControlDevices = params.controlDevices
  val logFullPedestrianHistory: Boolean = params.logFullPedestrianHistory
  val stop2Vertex: Stop2Vertex = params.stop2Vertex
  val rebuildTreeInterval: Option[Time] = params.rebuildTreeInterval
  val trackDensityInterval: Option[Time] = params.trackDensityInterval
  val resetFlowCountersInterval: Option[Time] = params.resetFlowCountersInterval
  val predictionInputParameters: PredictionInputParameters = params.predictionParameters

  val location = params.location
  val setup = params.setup


  val isPrediction: Boolean

  /*protected val inputParameters: SimulationInputParameters = {
    new SimulationInputParameters(st, et, motionModelUpdateInterval, updateRoutesInterval, spaceMicro, graph, stop2Vertex, controlDevices)
  }

  this.inputParameters.logFullPedestrianHistory = Some(this.logFullPedestrianHistory)
  this.inputParameters.timeTable = this.timeTable
  this.inputParameters.rebuildTreeInterval = this.rebuildTreeInterval
  this.inputParameters.stateEvaluationInterval = Some(this.stateEvaluationInterval)*/

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
  def intermediateDestinationReached: PedestrianSim => Boolean = p => {
    p.nextZone.isInside(p.currentPosition, p.isInsideAMW.isDefined)
  }

  /* Updates the next destination */
  val updateIntermediateDestination: (Time, PedestrianNOMAD) => Unit = (t, ped) => graph.processIntermediateArrival(t, ped)
  /* => graph match {
      case rm: RouteGraphMultiple[T] => { rm.processIntermediateArrival(ped) }
      case rs: RouteGraph[T] => { rs.processIntermediateArrival(ped) }
    }*/

  /* Computes the first route  */
  val setFirstRoute: (Time, PedestrianNOMAD) => Unit = {
    (t, ped) =>
      ped.appendAccomplishedRoute(this.currentTime, ped.origin, ped.currentPosition)
      graph match {
        case rm: MultipleGraph => {
          rm.setRouteFirst(t, ped)
        }
        case rs: SingleGraph => {
          rs.processIntermediateArrival(t, ped)
        }
      }
  }

  /* checks if the pedestrian has reached is final destination */
  def finalDestinationReached: PedestrianSim => Boolean = p => isInVertex(p.finalDestination)(p.currentPosition)


  /** Indicator wether the density should be measured */
  val measureDensity: Boolean = controlDevices.monitoredAreas.nonEmpty && controlDevices.amws.isEmpty && controlDevices.binaryGates.isEmpty && controlDevices.flowGates.isEmpty


  /** Indicator whether flow gates are present */
  val useFlowGates: Boolean = controlDevices.flowGates.nonEmpty


  /** Indicator whether binary gaets are present */
  val useBinaryGates: Boolean = controlDevices.binaryGates.nonEmpty


  /** Indicator whether an m-tree is used to perform neighbour search */
  val useTreeForNeighbourSearch: Boolean = rebuildTreeInterval.isDefined


  /** Indicator whether flow separators are used */
  val useFlowSep: Boolean = controlDevices.flowSeparators.nonEmpty


  /** Indicator whether AMW are used */
  val useAMWs: Boolean = controlDevices.amws.nonEmpty


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
  class StartSim(sim: NOMADGraphSimulator) extends super.GenericStartSim(sim) {

    override val priority: Int = Int.MaxValue

    override def execute(): Unit = {

      sim.eventLogger.trace("sim-time=" + sim.currentTime + ": simulation started. dt=" + sim.motionModelUpdateInterval)

      this.sim.graph.edges.foreach(e => e.updateCost(this.sim.currentTime, e.cost))

      // Inserts the update routes events in the simulation
      sim.insertEventWithZeroDelay(new UpdatePedestrianRoutes(sim))

      // Inserts the movement model
      sim.insertEventWithZeroDelay(new NOMADIntegrated(sim))

      // Inserts the event which computes the pedestrian density at regular intervals.
      if (!this.sim.isPrediction) {
        trackDensityInterval.foreach(interval => sim.insertEventWithDelay(interval)(new ComputePedestrianDensity(sim)))
        sim.insertEventWithZeroDelay(new ReinitializeFlowCounters(sim))
      }


      if (this.sim.controlDevices.amwsMode._1 == "static") {
        val engineeringPolicy = new StaticEngineeringSolution(sim.graph.vertexMapNew, sim.controlDevices.amws).staticPolicyEngineeringSolution
        sim.controlDevices.amws.filter(_.noControlPolicy).foreach(w => {
          val policy: Vector[AMWPolicy] = engineeringPolicy._1.x.collect { case p: AMWPolicy if p.name == w.name && p.start >= sim.startTime && p.end <= sim.finalTime => {
            p
          }
          }
          w.setControlPolicy(policy, engineeringPolicy._2.find(_.name == w.name))
          w.insertChangeSpeed(sim)
        })
      } else if (this.sim.controlDevices.amwsMode._1 == "reactive") {
        sim.controlDevices.amws.filter(_.noControlPolicy).foreach(w => {
          w.setControlPolicy(Vector(AMWPolicy(w.name, sim.startTime, sim.finalTime, w.speed(sim.currentTime), w.length)), None)
        })
        sim.controlDevices.amws.foreach(w => {
          w.insertChangeSpeed(sim)
        })

        if (sim.controlDevices.amws.nonEmpty && this.sim.controlDevices.amwsMode._1 == "reactive" && this.sim.controlDevices.amwsMode._2 == "density") {
          sim.trackDensityInterval.foreach(_ => sim.insertEventWithZeroDelay(new UpdateDensityReactiveAMWs(sim)))
        }

      }
      else if (this.sim.controlDevices.amwsMode._1 == "predictive") {
        sim.controlDevices.amws.filter(_.noControlPolicy).foreach(w => {
          w.setControlPolicy(Vector(AMWPolicy(w.name, sim.startTime, sim.finalTime, w.speed(sim.currentTime), w.length)), None)
          w.insertChangeSpeed(sim)
        })
      } else {
        throw new ControlDevicesException("Illegal amw mode ! ")
      }

      // Inserts the events for changing the AMW speeds.
      /*sim.controlDevices.amws.filter(_.noControlPolicy).foreach(w => {
        w.setControlPolicy(Vector(AMWPolicy(w.name, sim.startTime, sim.finalTime, w.speed(sim.currentTime), w.length)), None)
        w.insertChangeSpeed(sim)
      })*/


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
      //sim.insertEventWithZeroDelay(new SafeGuard())

    }

    type A = StartSim

    override def deepCopy(simulator: PedestrianPrediction): Option[A]

    = {
      None // Some(new StartSim(simulator))
    }
  }


  class EndSim(sim: NOMADGraphSimulator) extends GenericFinishSim(sim, sim.finalTime) {

    override def execute(): Any = {

      (sim.populationCompleted ++ sim.population).filter(_.accomplishedRoute.size > 1).foreach(ped =>
        ped.accomplishedRoute.sliding(2).foreach(leg => {

         /* val e: Option[MyEdge] = sim.graph.edges.find(e => e.startVertex == leg.head._2 && e.endVertex == leg.tail.head._2)
          if (e.isDefined && )*/

          sim.graph.addLinkTT(leg.head._2, leg.tail.head._2, leg.tail.head._1 - leg.head._1)
        })
      )


      sim.graph.edges.foreach(e => e.updateCost(sim.currentTime, e.cost))

    }

    type A = EndSim

    override def deepCopy(simulator: PedestrianPrediction): Option[A] = {
      Some(new EndSim(simulator))
    }
  }


  /**
    * Class to interrupt the simulation if some criteria is met. The idea is to prevent simulations running for
    * ridiculous times if some unfeasible situation has occured.
    */
  /*private class SafeGuard extends Action {

    override def execute(): Unit = {

      eventLogger.trace("sim-time=" + currentTime + ": safe guard execution")

      if (useFlowGates && controlDevices.flowGates.exists(fg => fg.pedestrianQueue.size > 100)) {
        abort(1)
      } else {
        insertEventWithDelay(Time(30))(new SafeGuard())
      }
    }

    type A = SafeGuard

    type B = NOMADGraphSimulator

    override def deepCopy(simulator: PedestrianPrediction): Option[A] = {
      Some(new SafeGuard)
    }
  }*/

  /**
    * Runs the simulation. This should be called after the processing events have been inserted.
    */
  override def run(): Unit = {
    super.genericRun(new StartSim(this), new EndSim(this))
  }


  def printSimulationInformation(): Unit = {
    logger.info("Simulator configuration:")


    if (measureDensity) {
      logger.info(" * measuring density")
    }

    if (useFlowSep && !controlDevices.fixedFlowSeparators) {
      logger.info(" * using dynamic flow separators")
    } else {
      logger.info(" * using static flow separators")
    }

    if (useTreeForNeighbourSearch) {
      logger.info(" * using m-tree for neighbour search")
    }

    if (useFlowGates) {
      logger.info(" * using flow gates")
    }

    if (useFlowSep) {
      logger.info(" * using accelerated moving walkways")
    }

    if (useBinaryGates) {
      logger.info(" * using binary gates")
    }
  }


  /**
    * Collects the parameters used for creating the simulation. The arguments are passed as a tuple.
    *
    * @return
    */
  /*@deprecated
  def getSetupArguments: SimulatorParameters = (
    startTime,
    finalTime,
    motionModelUpdateInterval,
    updateRoutesInterval,
    stateEvaluationInterval,
    rebuildTreeInterval,
    spaceMicro,
    graph,
    timeTable,
    this.stop2Vertex,
    controlDevices
  )*/

  def getSetupArgumentsNew: SimulationInputParameters = {
    {
      val inputParameters: SimulationInputParameters = {
        new SimulationInputParameters(startTime, finalTime, motionModelUpdateInterval, updateRoutesInterval, spaceMicro, graph, stop2Vertex, controlDevices, params.predictionParameters, "", "")
      }

      inputParameters.resetFlowCountersInterval = resetFlowCountersInterval
      inputParameters.trackDensityInterval = trackDensityInterval
      inputParameters.logFullPedestrianHistory = logFullPedestrianHistory
      inputParameters.timeTable = timeTable
      inputParameters.rebuildTreeInterval = rebuildTreeInterval
      inputParameters.stateEvaluationInterval = Some(stateEvaluationInterval)
      inputParameters
    }
  }

  /** Constructor which uses the most important parameters separately.
    *
    * @param st
    * @param et
    * @param motionModelUpdateInterval
    * @param updateRoutesInterval
    * @param stateEvaluationInterval
    * @param rebuildTreeInterval
    * @param spaceMicro
    * @param graph
    * @param timeTable
    * @param stop2Vertex
    * @param controlDevices
    * @param logFullPedestrianHistory
    */
  /*@deprecated
  def this(st: Time,
           et: Time,
           motionModelUpdateInterval: Time,
           updateRoutesInterval: Time,
           stateEvaluationInterval: Time,
           rebuildTreeInterval: Option[Time],
           spaceMicro: ContinuousSpace,
           graph: GraphContainer,
           timeTable: Option[PublicTransportSchedule],
           stop2Vertex: Stop2Vertex,
           controlDevices: ControlDevices,
           logFullPedestrianHistory: Boolean = false) = {
    this({
      val inputParameters: SimulationInputParameters = {
        new SimulationInputParameters(st, et, motionModelUpdateInterval, updateRoutesInterval, spaceMicro, graph, stop2Vertex, controlDevices)
      }

      inputParameters.logFullPedestrianHistory = logFullPedestrianHistory
      inputParameters.timeTable = timeTable
      inputParameters.rebuildTreeInterval = rebuildTreeInterval
      inputParameters.stateEvaluationInterval = Some(stateEvaluationInterval)
      inputParameters
    })
  }*/

}
