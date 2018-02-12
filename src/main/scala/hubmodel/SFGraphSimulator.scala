package hubmodel

import breeze.linalg.DenseVector
import hubmodel.input.demand.{InsertVehicleArrivals, PedestrianFlows, PedestrianGeneration, TimeTable}
import hubmodel.input.infrastructure.{BinaryGate, GraphReader, NodeNaming, ReadControlDevices, RouteGraph, SocialForceSpace, StartFlowGates}
import hubmodel.mgmt.EvaluateState
import hubmodel.mvmtmodels._
import hubmodel.route.UpdateRoutes
import hubmodel.tools.RebuildTree


class SFGraphSimulator(override val startTime: Time,
                       override val finalTime: Time,
                       val sf_dt: Time,
                       val evaluate_dt: Time,
                       val rebuildTreeInterval: Option[NewTime],
                       val spaceSF: SocialForceSpace,
                       val graph: RouteGraph,
                       val timeTable: TimeTable,
                       val pedestrianFlows: PedestrianFlows,
                       val nodeNaming: NodeNaming,
                       controlDevices: ReadControlDevices) extends PedestrianDES[PedestrianSim](startTime, finalTime) {

  /* checks whether a pedestrian has reach is next destination zone */
  def intermediateDestinationReached: PedestrianSim => Boolean = p => isInVertexNew(p.nextZone)(p.currentPositionNew)

  /* checks if the pedestrian has reached is final destination */
  def finalDestinationReached: PedestrianSim => Boolean = p => isInVertexNew(graph.vertexMap(p.dZone.toString))(p.currentPositionNew)

  /** Indicator whether flow gates are present */
  val useFlowGates: Boolean = graph.flowGates.nonEmpty && controlDevices.monitoredAreas.nonEmpty

  /** Indicator whether binary gaets are present */
  val useBinaryGates: Boolean = graph.binaryGates.nonEmpty && controlDevices.monitoredAreas.nonEmpty

  /* Using control */
  val useControl: Boolean = useFlowGates && useBinaryGates

  val useTreeForNeighbourSearch: Boolean = rebuildTreeInterval.nonEmpty

  /** KPIs */
  val criticalArea: List[VertexCell] = controlDevices.monitoredAreas.toList
  //List(VertexCell("CriticalZone1", DenseVector(50.0, 10.720), DenseVector( 56.0, 10.720), DenseVector( 56.0, 16.800), DenseVector( 50.0, 16.800)))
  //val criticalArea: List[Vertex] = List(Vertex("CriticalZone1", DenseVector(51.5, 10.72), DenseVector(80.40, 10.72), DenseVector(80.40, 16.80), DenseVector(51.50, 16.80)))
  val densityHistory: collection.mutable.ArrayBuffer[(Time, Double)] = collection.mutable.ArrayBuffer((startTime, 0.0))
  val inflowHistory: collection.mutable.ArrayBuffer[(Time, Double)] = collection.mutable.ArrayBuffer((startTime, 0.0))

  val closedEdges: scala.collection.mutable.HashSet[(VertexCell, VertexCell)] = scala.collection.mutable.HashSet()
  val gatesHistory: collection.mutable.ArrayBuffer[(Time, List[(String, Boolean)])] = collection.mutable.ArrayBuffer()


  class StartSim(sim: SFGraphSimulator) extends super.GenericStartSim(sim) {
    override def execute(): Unit = {
      sim.eventLogger.trace("sim-time=" + sim.currentTime + ": simulation started. dt=" + sim.sf_dt)
      sim.insertEventWithDelay(0)(new InsertVehicleArrivals(sim))
      sim.pedestrianFlows.flows.foreach(f => sim.insertEventWithDelay(0)(new PedestrianGeneration(f.O, f.D, new NewTime(f.start.toSecondOfDay - sim.startTime), new NewTime(f.end.toSecondOfDay - sim.startTime), f.f, sim)))
      sim.insertEventWithDelay(0)(new UpdateRoutes(sim))
      sim.insertEventWithDelay(0)(new NOMADOriginalModel(sim))
      if (sim.useControl) sim.insertEventWithDelay(0)(new EvaluateState(sim))
      if (sim.useFlowGates) sim.insertEventWithDelay(0)(new StartFlowGates(sim))
      if (sim.useTreeForNeighbourSearch) sim.insertEventAtAbsolute(0.0)(new RebuildTree(sim))
    }
  }

  override def run(): Unit = super.genericRun(new StartSim(this))


  /** Creates a string with the pedestrians who have exited the simulation as csv
    *
    */
  def printPopulationCompleted(): Option[String] = {
    if (this.populationCompleted.nonEmpty) Some(this.populationCompleted.tail.foldLeft(this.populationCompleted.head.toVisioSafeFormat()) { (s: String, p: PedestrianSim) => s + "\n" + p.toVisioSafeFormat() })
    else None
  }

}