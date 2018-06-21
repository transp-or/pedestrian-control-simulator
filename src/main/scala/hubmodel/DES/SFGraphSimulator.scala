package hubmodel.DES

import hubmodel.NewTimeNumeric.mkOrderingOps
import hubmodel._
import hubmodel.demand.{CreatePedestrian, PTInducedQueue, PedestrianFlowPT_New, PedestrianFlow_New, ProcessPedestrianFlows, ProcessTimeTable, PublicTransportSchedule, ReadPedestrianFlows, Stop2Vertices}
import hubmodel.mgmt.{ControlDevices, EvaluateState}
import hubmodel.mvmtmodels.NOMAD.NOMADOriginalModel
import hubmodel.mvmtmodels.RebuildTree
import hubmodel.ped.PedestrianSim
import hubmodel.route.UpdateRoutes
import hubmodel.supply.continuous.{ContinuousSpace, Wall}
import hubmodel.supply.graph.{RouteGraph, StartFlowGates, Stop2Vertex}
import hubmodel.supply.{NodeID_New, NodeParent, StopID_New, TrainID_New}
import hubmodel.tools.cells.{DensityMeasuredArea, Rectangle, isInVertex}

class SFGraphSimulator(override val startTime: Time,
                       override val finalTime: Time,
                       val sf_dt: Time,
                       val evaluate_dt: Time,
                       val rebuildTreeInterval: Option[Time],
                       private val spaceSF: ContinuousSpace,
                       val graph: RouteGraph,
                       val timeTable: PublicTransportSchedule,
                       val stop2Vertices: Stop2Vertex,
                       flows: (Iterable[PedestrianFlow_New], Iterable[PedestrianFlowPT_New]),
                       val controlDevices: ControlDevices) extends PedestrianDES[PedestrianSim](startTime, finalTime) {

  /* Stores the PT induced flows and the other flows separately*/
  val pedestrianFlows: Iterable[PedestrianFlow_New] = flows._1
  val pedestrianFlowsPT: Iterable[PedestrianFlowPT_New] = flows._2

  controlDevices.flowSeparators.foreach(_.initializePositionHistory(startTime))

  /* Access for the wall collection which is mostly contained in the SF infrastructrue file but some movable walls
  * are found in the graph infrastructure file */
  def walls: Iterable[Wall] = spaceSF.walls ++ controlDevices.flowSeparators.map(_.getWall)

  /* checks whether a pedestrian has reach is next destination zone */
  def intermediateDestinationReached: PedestrianSim => Boolean = p => isInVertex(p.nextZone)(p.currentPosition)

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

  /** Using control */
  val useControl: Boolean = useFlowGates || useBinaryGates
  if (useControl) {
    println(" * control strategies are used")
  }

  /** Indicator whether an m-tree is used to perform neighbour search */
  val useTreeForNeighbourSearch: Boolean = rebuildTreeInterval.isDefined
  if (useTreeForNeighbourSearch) {
    println(" * using m-tree for neighbour search")
  }


  /** KPIs */
  val criticalAreas: Map[String, DensityMeasuredArea] = controlDevices.monitoredAreas.map(zone => zone.name -> new DensityMeasuredArea(zone.name, zone.corners(0), zone.corners(1), zone.corners(2), zone.corners(3), startTime)).toMap

  //List(VertexCell("CriticalZone1", DenseVector(50.0, 10.720), DenseVector( 56.0, 10.720), DenseVector( 56.0, 16.800), DenseVector( 50.0, 16.800)))
  //val criticalArea: List[Vertex] = List(Vertex("CriticalZone1", DenseVector(51.5, 10.72), DenseVector(80.40, 10.72), DenseVector(80.40, 16.80), DenseVector(51.50, 16.80)))
  val densityHistory: collection.mutable.ArrayBuffer[(Time, Double)] = collection.mutable.ArrayBuffer((startTime, 0.0))
  val inflowHistory: collection.mutable.ArrayBuffer[(Time, Double)] = collection.mutable.ArrayBuffer((startTime, 0.0))

  val closedEdges: scala.collection.mutable.HashSet[(Rectangle, Rectangle)] = scala.collection.mutable.HashSet()
  val gatesHistory: collection.mutable.ArrayBuffer[(Time, List[(String, Boolean)])] = collection.mutable.ArrayBuffer()

  val PTInducedFlows: collection.mutable.Map[Rectangle, PTInducedQueue] = collection.mutable.Map()


  var regulatorIntegralAction: Double = 0.0
  /** Takes a conceptual node (train or on foot) and returns the set of "real" nodes (the ones used by the graph)
    * in which the pedestrians must be created.
    *
    * @param conceptualNode node representing the train or the pedestrian zone
    * @return iterable in which the pedestrians will be created
    */
  def conceptualNode2GraphNodes(conceptualNode: NodeParent): Iterable[Rectangle] = {
    conceptualNode match {
      case x: TrainID_New => this.stop2Vertices.stop2Vertices(this.timeTable.timeTable(x).stop).map(n => this.graph.vertexMap(n))
      case x: NodeID_New => Iterable(this.graph.vertexMap(x.ID))
      case x: StopID_New => Iterable(this.graph.vertexMap(x.ID.toString))
      case _ => throw new Exception("Track ID should not be there !")
    }
  }

  // check if vectors of nodeID are the same
  def isOnSamePlatform(t1: TrainID_New, t2: NodeParent): Boolean = {
    t2 match {
      case tID: TrainID_New => {
        this.stop2Vertices.stop2Vertices.get(this.timeTable.vehicle2Stop(t1)) match {
          case Some(x) => this.stop2Vertices.stop2Vertices.get(this.timeTable.vehicle2Stop(tID)) match {
            case Some(y) => x.sorted.zip(y.sorted).forall(p => p._1 == p._2)
            case None => false
          }
          case None => false
        }
        //train2Nodes(t1).get.sorted.zip(train2Nodes(t2).get.sorted).forall(p => p._1==p._2)
      }
      case _ => false
    }
  }


  def insertMultiplePedestrians(eventCollection: Iterable[(String, String, Time)]): Unit = {
    eventCollection
      .filter(ec => ((this.startTime <= ec._3 && ec._3 <= this.finalTime) || ec._3.value == math.pow(10,8)) && ec._1 != "-1" && ec._2 != "-1" && this.graph.vertexMap.keySet.contains(ec._1.drop(2)) && this.graph.vertexMap.keySet.contains(ec._2.drop(2)))
      .foreach(ec => {
        if (ec._1.contains("z_") && ec._2.contains("z_")) {
          this.eventList += new MyEvent(ec._3, new CreatePedestrian(graph.vertexMap(ec._1.drop(2)), graph.vertexMap(ec._2.drop(2)), this))
        } else if (ec._1.contains("t_") && ec._2.contains("z_")) {
          this.timeTable.timeTable.find(_._1.ID == ec._1.drop(2)).get._2.addAlightingPassenger(StopID_New(ec._2.drop(2).toInt, ""))
        } else if (ec._1.contains("t_") && ec._2.contains("t_")) {
          this.timeTable.timeTable.find(_._1.ID == ec._1.drop(2)).get._2.addAlightingPassenger(TrainID_New(ec._2.drop(2), ""))
        } else if (ec._1.contains("z_") && ec._2.contains("t_")) {
          this.eventList += new MyEvent(ec._3, new CreatePedestrian(graph.vertexMap(ec._1.drop(2)), graph.vertexMap(ec._2.drop(2)), this))
        } else {
          this.eventList += new MyEvent(ec._3, new CreatePedestrian(graph.vertexMap(ec._1.drop(2)), graph.vertexMap(ec._2.drop(2)), this))
        }
      })
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
      if (sim.useTreeForNeighbourSearch) sim.insertEventWithDelayNew(new Time(0.0))(new RebuildTree(sim))
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