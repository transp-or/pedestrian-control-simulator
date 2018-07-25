import java.io.{BufferedWriter, File, FileWriter}

import breeze.numerics.{floor, round}
import com.typesafe.config.{Config, ConfigFactory}
import hubmodel.DES.SFGraphSimulator
import hubmodel.demand.{PedestrianFlowFunction_New, PedestrianFlowPT_New, PedestrianFlow_New, PublicTransportSchedule, readDisaggDemand, readDisaggDemandTF, readPedestrianFlows, readSchedule, readScheduleTF}
import hubmodel.output.image.{DrawControlDevicesAndWalls, DrawGraph, DrawWalls, DrawWallsAndGraph}
import hubmodel.output.video.MovingPedestriansWithDensityWithWallVideo
import hubmodel.ped.{PedestrianSim, PedestrianTrait}
import hubmodel.supply.continuous.{MovableWall, ReadContinuousSpace}
import hubmodel.supply.graph.{BinaryGate, Stop2Vertex, readGraph, readStop2Vertex}
import hubmodel.tools.cells.DensityMeasuredArea
import myscala.math.vector.{Vector2D, Vector3D}
import myscala.timeBlock
import play.api.libs.json.Reads._
import play.api.libs.json._
/**
  * Created by nicholas on 5/12/17.
  */

package object hubmodel {

  /* pedestrian isolations */
  val ISOLATED: Int = 0
  val IN_RANGE: Int = 1
  val IN_COLLISION: Int = 2

  /* PARAMETERS */

  // Length in meters of the othogonal extension of the "near region" for flow lines. This shouls be larger than the
  // maximum walking speed * the motion model clock (sf_dt) to ensure a pedestrian cannot "jump" over it. The larger the
  // value of the parameter, the more pedestrians will be checked to see if they cross the line..
  val FLOW_LINE_REGION_EXTENSION: Double = 2.0

  // Change in opposing flow fractions which is considered significant. The flow separators will only move if the change
  // is larger than this value
  val FLOW_SEPARATOR_UPDATE: Double = 0.1 // 10%

  type VehicleID = String
  type VertexID = String
  type StopID = String

  type Position = Vector2D
  type Direction = Vector2D
  type Velocity = Vector2D
  type Acceleration = Vector2D
  type Force = Vector2D

  def distance(a: Vector2D, b: Vector2D): Double = scala.math.pow((b.X - a.X) * (b.X - a.X) + (b.Y - a.Y) * (b.Y - a.Y), 0.5)

  def distance(a: Vector3D, b: Vector3D): Double = scala.math.pow((b.X - a.X) * (b.X - a.X) + (b.Y - a.Y) * (b.Y - a.Y) + (b.Z - a.Z) * (b.Z - a.Z), 0.5)


  class Time(val value: Double) extends AnyVal {

    def +(m: Time): Time = new Time(this.value + m.value)

    def addDouble(m: Double): Time = new Time(this.value + m)

    def -(m: Time): Time = new Time(this.value - m.value)

    def abs: Time = new Time(java.lang.Math.abs(this.value))

    def asReadable: String = {
      val hours: Int = floor(value / 3600.0).toInt
      val minutes: Int = floor((value - hours * 3600) / 60.0).toInt
      val seconds: Double = floor(value - hours * 3600 - minutes * 60)
      hours.toString + ":" + minutes.toString + ":" + seconds.toString
    }

    def asVisioSafe: String = {
      val h: Int = floor(this.value / 3600).toInt
      val min: Int = floor((this.value - h * 3600) / 60).toInt
      val s: Int = floor(this.value - 3600 * h - 60 * min).toInt
      val ms: Int = round(1000 * (this.value - 3600 * h - 60 * min - s)).toInt
      h.toString + "," + min.toString + "," + s.toString + "," + ms.toString
    }

    override def toString: String = value.toString
  }

  object Time {
    def apply(value: Double): Time = new Time(value)

    def fromDouble(v: Double): Time = {
      new Time(v)
    }

    implicit def orderingByValue: Ordering[Time] = {
      Ordering.by(t => t.value)
    }

    implicit val readerTimeFromString: Reads[Time] = Reads.of[String].map(s => Time(s.toDouble))

  }

  object TimeNumeric extends Ordering[Time] {
    def compare(x: Time, y: Time): Int = x.value compare y.value
  }



  /** Generation of a UUID. This can be used to generate unique identifiers for objects.
    *
    * @return UUID formatted as a String
    */
  def generateUUID: String = java.util.UUID.randomUUID.toString


  def writePopulationTrajectories(population: Iterable[PedestrianTrait], file: String): Unit = {
    val f = new File(file)
    val bw = new BufferedWriter(new FileWriter(f))
    var counter: Int = 0
    val totalPeds: Int = population.size
    for (p <- population) {
      counter += 1
      print(counter + "/" + totalPeds + " pedestrians processed\r")
      bw.write(p.toVisioSafeFormat().stripLineEnd)
      bw.write("\n")
    }
    bw.close()
  }

  /** Creates a simulation, but does not run it
    *
    * @return simulator ready to run
    */
  def createSimulation(config: Config): SFGraphSimulator = {


    // checkValid(), just as in the plain SimpleLibContext.
    // Note that these fields are NOT lazy, because if we're going to
    // get any exceptions, we want to get them on startup.
    config.checkValid(ConfigFactory.defaultReference())

    // ******************************************************************************************
    //                        Loads the input data
    // ******************************************************************************************

    println("Reading and creating initial data")

    // Builds the set of walls used by the Social force model
    val infraSF = new ReadContinuousSpace(config.getString("files.walls"))

    // Builds the graph used for route choice. This Graph is coposed of multiple different link types.
    val (routeGraph, controlDevices) = readGraph(
      config.getString("files.graph"),
      config.getBoolean("sim.use_flow_gates"),
      config.getBoolean("sim.use_binary_gates"),
      config.getBoolean("sim.use_amw"),
      config.getBoolean("sim.use_flow_sep"),
      config.getBoolean("sim.measure_density")
    )

    // Loads the pedestrian flows. These are either exogenous to the trains (from outside) or flows originating from trains.
    val flows: (Iterable[PedestrianFlow_New], Iterable[PedestrianFlowPT_New], Iterable[PedestrianFlowFunction_New]) = if (!config.getIsNull("files.flows") && config.getBoolean("sim.use_flows")) {
      readPedestrianFlows(config.getString("files.flows"))
    } else if (!config.getIsNull("files.flows_TF") && config.getBoolean("sim.use_flows")) {
      readPedestrianFlows(config.getString("files.flows_TF"))
    } else {
      println(" * using only disaggregate pedestrian demand")
      (Iterable(), Iterable(), Iterable())
    }


    // Loads the train time table used to create demand from trains
    val (timeTable, stop2Vertex) = if (config.hasPath("files.timetable") && !config.getIsNull("files.timetable")) {
      (readSchedule(config.getString("files.timetable")),
        readStop2Vertex(config.getString("files.zones_to_vertices_map")))
    } else if (config.hasPath("files.timetable_TF") && !config.getIsNull("files.timetable_TF")) {
      (readScheduleTF(config.getString("files.timetable_TF")),
        readStop2Vertex(config.getString("files.zones_to_vertices_map")))
    } else if (flows._2.isEmpty) {
      println(" * no time table is required as PT induced flows are empty")
      (new PublicTransportSchedule("unused", Vector()), new Stop2Vertex(Map(), Map()))
    } else {
      throw new IllegalArgumentException("both time tables files are set to null in config file")
    }

    //val flows =  new ReadPedestrianFlows(config.getString("files.flows"), config.getBoolean("sim.use_flows"))

    // Loads the disaggregate pedestrian demand.
    val disaggPopulation: Iterable[(String, String, Time)] = if (config.hasPath("files.flows_TF") && config.getIsNull("files.flows_TF") && !config.getIsNull("files.disaggregate_demand")) {
      readDisaggDemand(config.getString("files.disaggregate_demand"))
    } else if (config.hasPath("files.disaggregate_demand") && config.getIsNull("files.disaggregate_demand") && !config.getIsNull("files.flows_TF")) {
      readDisaggDemandTF(config.getString("files.flows_TF"))
    } else {
      println(" * using only standard pedestrian flows")
      Iterable()
    }

    // Loads the start time, end time and time intervals
    val simulationStartTime: Time = Time(config.getDouble("sim.start_time"))
    val simulationEndTime: Time = Time(config.getDouble("sim.end_time"))
    val socialForceInterval: Time = Time(config.getDouble("sim.sf_dt"))
    val evaluationInterval: Time = Time(config.getDouble("sim.evaluate_dt"))
    val rebuildTreeInterval: Time = Time(config.getDouble("sim.rebuild_tree_dt"))

    // number of simulations to run
    val n: Int = config.getInt("sim.nb_runs")



    val sim: SFGraphSimulator = new SFGraphSimulator(
      startTime = simulationStartTime,
      finalTime = simulationEndTime,
      sf_dt = socialForceInterval,
      evaluate_dt = evaluationInterval,
      rebuildTreeInterval = Some(rebuildTreeInterval),
      spaceSF = infraSF.continuousSpace,
      graph = routeGraph,
      timeTable = timeTable,
      stop2Vertices = stop2Vertex,
      flows = flows,
      controlDevices = controlDevices
    )

    if (disaggPopulation.nonEmpty) {
      sim.insertMultiplePedestrians(disaggPopulation)//.filter({p => if (ThreadLocalRandom.current.nextDouble(0.0, 1.0) > 0.6 )true else false}) )
    }

    sim
  }


  // ******************************************************************************************
  //                    Runs the simulation, creates video and outputs results
  // ******************************************************************************************

  case class ResultsContainerNew(exitCode:Int, completedPeds: Vector[PedestrianSim], uncompletedPeds: Vector[PedestrianSim], densityZones: Map[String, DensityMeasuredArea])

  // Container for the results from a simulation. This type chould be modified if the collectResults function is modified
  type ResultsContainer = (Vector[PedestrianSim], Map[String, DensityMeasuredArea], Vector[PedestrianSim])

  /** Used to extract the desired results from the simulator. Avoids keeping all information in memory.
    *
    * The current results are:
    *  - population which completed their trips
    *  - density in zones
    *  - inflow in zones
    *
    * @param simulator simulator from which to extract the results
    * @return results from the simulation
    */
  def collectResults(simulator: SFGraphSimulator): ResultsContainerNew = {
    if (simulator.exitCode == 0) {
      ResultsContainerNew(
        simulator.exitCode,
        simulator.populationCompleted,
        simulator.population.toVector,
        simulator.criticalAreas
      )
    } else {
      ResultsContainerNew(simulator.exitCode, Vector(), Vector(), Map())
    }
  }

  /** Creates, runs and makes a video from the simulation. No results are processed.
    * Making the video can take some time.
    */
  def runSimulationWithVideo(config: Config): ResultsContainerNew = {

    // create simulation
    val sim = createSimulation(config)

    // Creates images representing the walls, route graph and both overlaid.
    val wallsImage = new DrawWalls(sim.walls, config.getString("output.output_prefix") + "_wallsWithNames.png", showNames = true)
    val graphImage = new DrawGraph(sim.graph.edges.map(e => (e.startVertex, e.endVertex)).toVector, config.getString("output.output_prefix") + "_graph.png")
    val fullImage = new DrawWallsAndGraph(sim.walls, sim.graph.edges.map(e => (e.startVertex, e.endVertex)).toVector, config.getString("output.output_prefix") + "_wallsAndGraph.png")
    val devicesImage = new DrawControlDevicesAndWalls(config.getString("output.output_prefix") + "_wallsWithDevices.png", sim.walls, sim.controlDevices)

    println("Running simulation for video...")

    // execute simulation
    timeBlock(sim.run())

    println("Making video of simulation, this can take some time...")

    val gates: List[BinaryGate] = List()

    new MovingPedestriansWithDensityWithWallVideo(
      config.getString("output.output_prefix") + "_moving_pedestrians_walls.mp4",
      sim.walls.filterNot(_.isInstanceOf[MovableWall]),
      math.max((1.0 / config.getDouble("output.video_dt")).toInt, 1),
      sim.populationCompleted ++ sim.population,
      sim.criticalAreas.values,
      gates.map(g => g.ID -> g).toMap,
      sim.gatesHistory.map(p => ((p._1.value * 1).round.toInt, p._2)),
      sim.densityHistory.map(p => ((p._1.value * 1).round.toInt, p._2)),
      (sim.startTime.value to sim.finalTime.value by config.getDouble("output.video_dt")).map(new Time(_)),
      sim.controlDevices.flowSeparators
    )


    /*if (config.getBoolean("output.write_trajectories_as_VS")) {
      println("Writing trajectory data from video...")
      sim.writePopulationTrajectories(config.getString("output.output_prefix") + "_simulation_trajectories.csv")
    }*/

    collectResults(sim)
  }

  /** Runs the simulation and then collects the results. The simulation is timed.
    *
    * @param simulator simulation to run
    * @return results collected from the simulation
    */
  def runAndCollect(simulator: SFGraphSimulator): ResultsContainerNew =  {
    timeBlock(simulator.run())
    collectResults(simulator)
  }

}
