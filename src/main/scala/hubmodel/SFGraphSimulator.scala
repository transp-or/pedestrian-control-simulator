package hubmodel

import hubmodel.demand.{CreatePedestrian, PTInducedQueue, PedestrianFlows, ProcessPedestrianFlows, ProcessTimeTable, TimeTable}
import hubmodel.mgmt.EvaluateState
import hubmodel.mvmtmodels._
import hubmodel.route.UpdateRoutes
import hubmodel.supply.{ControlDevices, NodeID_New, NodeParent, RouteGraph, SocialForceSpace, StartFlowGates, TrainID_New}
import hubmodel.tools.RebuildTree
import hubmodel.NewTimeNumeric.mkOrderingOps


class SFGraphSimulator(override val startTime: NewTime,
                       override val finalTime: NewTime,
                       val sf_dt: NewTime,
                       val evaluate_dt: NewTime,
                       val rebuildTreeInterval: Option[NewTime],
                       val spaceSF: SocialForceSpace,
                       val graph: RouteGraph,
                       val timeTable: TimeTable,
                       val pedestrianFlows: PedestrianFlows,
                       val controlDevices: ControlDevices) extends PedestrianDES[PedestrianSim](startTime, finalTime) {

  /* checks whether a pedestrian has reach is next destination zone */
  def intermediateDestinationReached: PedestrianSim => Boolean = p => isInVertex(p.nextZone)(p.currentPositionNew)

  /* checks if the pedestrian has reached is final destination */
  def finalDestinationReached: PedestrianSim => Boolean = p => isInVertex(p.dZone)(p.currentPositionNew)

  println("Simulator configuration:")

  /** Indicator wether the density should be measured */
  val measureDensity: Boolean = controlDevices.monitoredAreas.nonEmpty && controlDevices.amws.isEmpty && controlDevices.binaryGates.isEmpty && controlDevices.flowGates.isEmpty
  if (measureDensity) {println( " * measuring density")}

  /** Indicator whether flow gates are present */
  val useFlowGates: Boolean = controlDevices.flowGates.nonEmpty
  if (useFlowGates) {println( " * using flow gates")}

  /** Indicator whether binary gaets are present */
  val useBinaryGates: Boolean = controlDevices.binaryGates.nonEmpty
  if (useBinaryGates) {println( " * using binary gates")}

  /** Using control */
  val useControl: Boolean = useFlowGates || useBinaryGates
  if (useControl) {println( " * control strategies are used")}

  /** Indicator whether an m-tree is used to perform neighbour search */
  val useTreeForNeighbourSearch: Boolean = rebuildTreeInterval.isDefined
  if (useTreeForNeighbourSearch) {println( " * using m-tree for neighbour search")}


  /** KPIs */
  val criticalArea: List[VertexRectangle] = controlDevices.monitoredAreas.toList
  //List(VertexCell("CriticalZone1", DenseVector(50.0, 10.720), DenseVector( 56.0, 10.720), DenseVector( 56.0, 16.800), DenseVector( 50.0, 16.800)))
  //val criticalArea: List[Vertex] = List(Vertex("CriticalZone1", DenseVector(51.5, 10.72), DenseVector(80.40, 10.72), DenseVector(80.40, 16.80), DenseVector(51.50, 16.80)))
  val densityHistory: collection.mutable.ArrayBuffer[(NewTime, Double)] = collection.mutable.ArrayBuffer((startTime, 0.0))
  val inflowHistory: collection.mutable.ArrayBuffer[(NewTime, Double)] = collection.mutable.ArrayBuffer((startTime, 0.0))

  val closedEdges: scala.collection.mutable.HashSet[(VertexRectangle, VertexRectangle)] = scala.collection.mutable.HashSet()
  val gatesHistory: collection.mutable.ArrayBuffer[(NewTime, List[(String, Boolean)])] = collection.mutable.ArrayBuffer()

  val PTInducedFlows: collection.mutable.Map[VertexRectangle, PTInducedQueue] = collection.mutable.Map()

  /** Takes a conceptual node (train or on foot) and returns the set of "real" nodes (the ones used by the graph)
    * in which the pedestrians must be created.
    *
    * @param conceptualNode node representing the train or the pedestrian zone
    * @return iterable in which the pedestrians will be created
    */
  def conceptualNode2GraphNodes(conceptualNode: NodeParent): Iterable[VertexRectangle] = {
    conceptualNode match {
      case x: TrainID_New => this.timeTable.train2NodesNew(x).map(n => this.graph.vertexMap(n.ID))
      case x: NodeID_New => Iterable(this.graph.vertexMap(x.ID))
      case _ => throw new Exception ("Track ID should not be there !")
    }
  }

  def insertMultiplePedestrians(eventCollection: Iterable[(String, String, NewTime)]): Unit = {
    eventCollection.filter(ec => this.startTime <= ec._3 && ec._3 <= this.finalTime && ec._1 != "-1" && ec._2 != "-1").foreach(ec => this.eventList += new MyEvent(ec._3, new CreatePedestrian(graph.vertexMap(ec._1), graph.vertexMap(ec._2), this)))
  }




  class StartSim(sim: SFGraphSimulator) extends super.GenericStartSim(sim) {
    override def execute(): Unit = {
      sim.eventLogger.trace("sim-time=" + sim.currentTime + ": simulation started. dt=" + sim.sf_dt)
      sim.insertEventWithZeroDelay(new ProcessTimeTable(sim))
      sim.insertEventWithZeroDelay(new ProcessPedestrianFlows(sim))
      sim.insertEventWithZeroDelay(new UpdateRoutes(sim))
      sim.insertEventWithZeroDelay(new NOMADOriginalModel(sim))
      if (sim.useControl) sim.insertEventWithZeroDelay(new EvaluateState(sim))
      else if (sim.measureDensity && !sim.useControl) sim.insertEventWithZeroDelay(new EvaluateState(sim))
      if (sim.useFlowGates) sim.insertEventWithZeroDelay(new StartFlowGates(sim))
      if (sim.useTreeForNeighbourSearch) sim.insertEventWithDelayNew(new NewTime(0.0))(new RebuildTree(sim))
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