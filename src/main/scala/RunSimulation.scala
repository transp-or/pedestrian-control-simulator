import breeze.linalg.DenseVector
import breeze.numerics.pow
import com.typesafe.config.{Config, ConfigFactory}
import hubmodel.DES.SFGraphSimulator
import hubmodel._
import hubmodel.demand.{PedestrianFlowPT_New, PedestrianFlow_New, ReadDisaggDemand, readDisaggDemand, readDisaggDemandTF, readPedestrianFlows, readSchedule, readScheduleTF}
import hubmodel.output.TRANSFORM.PopulationProcessingTRANSFORM
import hubmodel.output.image.{DrawGraph, DrawWalls, DrawWallsAndGraph}
import hubmodel.output.video.MovingPedestriansWithDensityWithWallVideo
import hubmodel.ped.PedestrianSim
import hubmodel.supply.graph.{BinaryGate, readGraph, readStop2Vertex}
import hubmodel.supply.continuous.ReadContinuousSpace
import hubmodel.results.PopulationProcessing
import myscala.math.stats.{ComputeStats, stats}
import myscala.output.SeqOfSeqExtensions.SeqOfSeqWriter
import myscala.output.SeqTuplesExtensions.SeqTuplesWriter
import myscala.timeBlock

import scala.collection.JavaConverters._
import scala.collection.parallel.ForkJoinTaskSupport

/**
  * Runs the simulations based on the configuration file. This configuration file contains all the details regarding
  * the eventual management strategies to use, the number of runs to perform, the simulation time steps and the
  * processing of the results. The keys steps for beaing able to run a simulation are the following:
  *
  */
object RunSimulation extends App {

  // ******************************************************************************************
  //                    Read CLI arguments and process parameters file
  // ******************************************************************************************

  // Used to parse command line inputs
  case class CLInput(conf: String = "")

  // Actually parses the command line arguments
  val parser = new scopt.OptionParser[CLInput]("scopt") {
    head("scopt", "3.x")

    opt[String]('c', "conf").required().valueName("<file>")
      .action((x, c) => c.copy(conf = x))
      .text("required, configuration file for the simulation")

    help("help").text("prints this usage text")
  }

  // Process the file passed as input and checks the format and parameters
  val confFile: String = parser.parse(args, CLInput()) match {
    case Some(conf) =>
      if (conf.conf.isEmpty) {
        println("Empty conf file, defaulting to reference.conf")
        "reference.conf"
      }
      else {
        conf.conf
      }
    case None =>
      println("Error parsing CLI arguments, defaulting to reference.conf")
      "reference.conf"
  }

  // Reads the file passed as argument
  val config: Config = ConfigFactory.load(confFile)

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


  // Loads the train time table used to create demand from trains
  val (timeTable, stop2Vertex) = if (!config.getIsNull("files.timetable")) {
    (readSchedule(config.getString("files.timetable")),
      readStop2Vertex(config.getString("files.zones_to_vertices_map")))
  } else if (!config.getIsNull("files.timetable_TF")) {
    (readScheduleTF(config.getString("files.timetable_TF")),
      readStop2Vertex(config.getString("files.zones_to_vertices_map")))
  } else {
    throw new IllegalArgumentException("both time tables files are set to null in config file")
  }

  // Loads the pedestrian flows. These are either exogenous to the trains (from outside) or flows originating from trains.
  val flows: (Iterable[PedestrianFlow_New], Iterable[PedestrianFlowPT_New]) = if (!config.getIsNull("files.flows") && config.getBoolean("sim.use_flows")) {
    readPedestrianFlows(config.getString("files.flows"))
  } else if (!config.getIsNull("files.flows_TF") && config.getBoolean("sim.use_flows")) {
    readPedestrianFlows(config.getString("files.flows_TF"))
  } else {
    println(" * using only disaggregate pedestrian demand")
    (Iterable(), Iterable())
  }

  //val flows =  new ReadPedestrianFlows(config.getString("files.flows"), config.getBoolean("sim.use_flows"))

  // Loads the disaggregate pedestrian demand.
  val disaggPopulation: Iterable[(String, String, Time)] = if (config.getIsNull("files.flows_TF") && !config.getIsNull("files.disaggregate_demand")) {
    readDisaggDemand(config.getString("files.disaggregate_demand"))
  } else if (config.getIsNull("files.disaggregate_demand") && !config.getIsNull("files.flows_TF")) {
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

  val runSimulationsInParallel: Boolean = config.getBoolean("execution.parallel")

  // ******************************************************************************************
  //                    Runs the simulation, creates video and outputs results
  // ******************************************************************************************

  println("Running " + n + " simulations")

  // Container for the results from a simulation. This type chould be modified if the collectResults function is modified
  type ResultsContainer = (Vector[PedestrianSim], List[(Time, Double)], List[(Time, Double)])

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
  def collectResults(simulator: SFGraphSimulator): ResultsContainer = {
    (
      simulator.populationCompleted,
      simulator.densityHistory.toList,
      simulator.inflowHistory.toList
    )
  }

  /** Creates a simulation, but does not run it
    *
    * @return simulator ready to run
    */
  def createSimulation(): SFGraphSimulator = {
    new SFGraphSimulator(
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
  }

  /** Creates, runs and makes a video from the simulation. No results are processed.
    * Making the video can take some time.
    */
  def runSimulationWithVideo(): ResultsContainer = {

    // create simulation
    val sim = createSimulation()

    if (disaggPopulation.nonEmpty) {
      sim.insertMultiplePedestrians(disaggPopulation)
    }

    println("Running simulation for video...")

    // execute simulation
    timeBlock(sim.run())

    println("Making video of simulation, this can take some time...")

    val gates: List[BinaryGate] = List()

    /*new MovingPedestriansWithDensityVideo(
      config.getString("output.output_prefix") + "_moving_pedestrians.mp4",
      if (config.getString("output.bckg_image_video") == "None") {
        None
      } else {
        Option(config.getString("output.bckg_image_video"))
      },
      (config.getDouble("output.bckg_image_width"), config.getDouble("output.bckg_image_height")),
      (1.0 / 0.2).toInt,
      sim.populationCompleted ++ sim.population,
      sim.criticalArea,
      gates.map(g => g.ID -> g).toMap,
      sim.gatesHistory.map(p => ((p._1 * 1).round.toInt, p._2)),
      sim.densityHistory.map(p => ((p._1 * 1).round.toInt, p._2)),
      (simulationStartTime to simulationEndTime by 0.2).toVector
    )*/

    // Creates images representing the walls, route graph and both overlaid.
    val wallsImage = new DrawWalls(sim.walls, config.getString("output.output_prefix") + "_wallsWithNames.png", showNames = true)
    val graphImage = new DrawGraph(sim.graph.edges.map(e => (e.startVertex, e.endVertex)).toVector, config.getString("output.output_prefix") + "_graph.png")
    val fullImage = new DrawWallsAndGraph(sim.walls, sim.graph.edges.map(e => (e.startVertex, e.endVertex)).toVector, config.getString("output.output_prefix") + "_wallsAndGraph.png")


    new MovingPedestriansWithDensityWithWallVideo(
      config.getString("output.output_prefix") + "_moving_pedestrians_walls.mp4",
      sim.walls,
      math.max((1.0 / config.getDouble("output.video_dt")).toInt, 1),
      sim.populationCompleted ++ sim.population,
      sim.criticalArea,
      gates.map(g => g.ID -> g).toMap,
      sim.gatesHistory.map(p => ((p._1.value * 1).round.toInt, p._2)),
      sim.densityHistory.map(p => ((p._1.value * 1).round.toInt, p._2)),
      (simulationStartTime.value to simulationEndTime.value by config.getDouble("output.video_dt")).map(new Time(_))
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
  def runAndCollect(simulator: SFGraphSimulator): ResultsContainer = {

    if (disaggPopulation.nonEmpty) {
      simulator.insertMultiplePedestrians(disaggPopulation)
    }

    timeBlock(simulator.run())
    collectResults(simulator)
  }

  // Runs the simulations in parallel or sequential based on the config file.
  val results: Vector[ResultsContainer] = {
    if (config.getBoolean("output.make_video") && n == 0) {
      Vector(runSimulationWithVideo())
    }
    else if (config.getBoolean("output.make_video") && n > 0 && runSimulationsInParallel) {
      val simulationCollection: collection.parallel.ParSeq[SFGraphSimulator] = collection.parallel.ParSeq.fill(n - 1)(createSimulation())
      simulationCollection.tasksupport = new ForkJoinTaskSupport(new java.util.concurrent.ForkJoinPool(config.getInt("execution.threads")))
      simulationCollection.par.map(runAndCollect).seq.toVector :+ runSimulationWithVideo()
    }
    else if (config.getBoolean("output.make_video") && n > 0 && !runSimulationsInParallel) {
      val simulationCollection: collection.immutable.Vector[SFGraphSimulator] = collection.immutable.Vector.fill(n - 1)(createSimulation())
      simulationCollection.map(runAndCollect).seq.toVector :+ runSimulationWithVideo()
    }
    else if (!config.getBoolean("output.make_video") && n > 0 && runSimulationsInParallel) {
      val simulationCollection: collection.parallel.ParSeq[SFGraphSimulator] = collection.parallel.ParSeq.fill(n)(createSimulation())
      simulationCollection.tasksupport = new ForkJoinTaskSupport(new java.util.concurrent.ForkJoinPool(config.getInt("execution.threads")))
      simulationCollection.par.map(runAndCollect).seq.toVector
    }
    else {
      val simulationCollection: collection.immutable.Vector[SFGraphSimulator] = collection.immutable.Vector.fill(n)(createSimulation())
      simulationCollection.map(runAndCollect)
    }
  }

  // ******************************************************************************************
  //                           Processes and writes results to CSV
  // ******************************************************************************************

  if (config.getBoolean("output.write_travel_times") || config.getBoolean("output.write_densities") || config.getBoolean("output.write_tt_stats") || config.getBoolean("output.write_inflow")) {

    println("Processing results")

    // Collects times at which densities where measured
    val densityTimes: Vector[Time] = results.head._2.unzip._1.toVector
    val inflowTimes: Vector[Time] = results.head._3.unzip._1.toVector

    // compute mean and variance of density
    val meanDensity: DenseVector[Double] = results.map(res => DenseVector(res._2.unzip._2.toArray)).foldLeft(DenseVector.fill(results.head._2.size)(0.0)) { (old: DenseVector[Double], n: DenseVector[Double]) => old + (n / results.size.toDouble) }
    val varianceDensity: DenseVector[Double] = results.map(res => DenseVector(res._2.unzip._2.toArray)).foldLeft(DenseVector.fill(results.head._2.size)(0.0)) { (old: DenseVector[Double], n: DenseVector[Double]) => old + (pow(n - meanDensity, 2) / (results.size.toDouble - 1.0)) }.map(math.pow(_, 0.5))

    // Collects then writes individual travel times to csv
    if (config.getBoolean("output.write_travel_times")) results
      .map(r => r._1.map(p => p.travelTime.value))
      .writeToCSV(
        config.getString("output.output_prefix") + "_travel_times.csv",
        columnNames = Some(Vector.fill(results.size)("r").zipWithIndex.map(t => t._1 + t._2.toString)),
        rowNames = None
      )

    // Collects then writes individual travel times with OD to csv
    if (config.getBoolean("output.write_travel_times")) results
      .zipWithIndex
      .flatMap(r => r._1._1.map(p => (r._2, p.travelTime, p.origin, p.finalDestination)))
      .writeToCSV(
        config.getString("output.output_prefix") + "_travel_times_OD.csv",
        columnNames = Some(Vector("run", "travel_time", "origin_id", "destination_id")),
        rowNames = None
      )


    // writes densities to csv, first column is time, second column is mean, third column is var, then all individual densities
    if (config.getBoolean("output.write_densities")) (densityTimes +: meanDensity.toScalaVector() +: varianceDensity.toScalaVector() +: results.map(_._2.map(_._2).toVector))
      .writeToCSV(
        config.getString("output.output_prefix") + "_densities.csv",
        rowNames = None,
        columnNames = Some(Vector("time", "mean", "variance") ++ Vector.fill(results.size)("r").zipWithIndex.map(t => t._1 + t._2.toString)
        )
      )

    // computes statistics on travel times and writes them
    if (config.getBoolean("output.write_tt_stats")) results.map(r => stats(r._1.map(p => p.travelTime.value))).writeToCSV(config.getString("output.output_prefix") + "_travel_times_stats.csv", rowNames = None, columnNames = Some(Vector("size", "mean", "variance", "median", "min", "max")))

    // collects and writes inflow values to csv
    if (config.getBoolean("output.write_inflow")) (inflowTimes +: results.map(d => d._3.map(_._2).toVector)).writeToCSV(config.getString("output.output_prefix") + "_inflow.csv")
  }

  if (!config.getBoolean("output.make_video") && config.getBoolean("output.write_trajectories_as_VS")) {
    println("Writing trajectories as VS to file")
    writePopulationTrajectories(results.head._1, config.getString("output.output_prefix") + "_simulation_trajectories.csv")
  }

  /*{
    val ODPairsToAnalyse: Iterable[(String, String)] = config.getStringList("results-analysis.o_nodes").asScala.zip(config.getStringList("results-analysis.d_nodes").asScala).map(t => (t._1, t._2))

    def findInterval(t: Double, times: Vector[Double]): Int = {
      times.indexWhere(_ > t)
    }

    def pedData: PedestrianSim => Double = ped => ped.travelTime.value

    def pedWindows: PedestrianSim => Int = ped => findInterval(ped.entryTime.value, (simulationStartTime.value to simulationEndTime.value by 60.0).toVector)

    def pedFilter: PedestrianSim => Boolean = ped => ODPairsToAnalyse.exists(_ == (ped.origin.name, ped.finalDestination.name))

    results.foreach(r => {
      val res = r._1.aggregateMetricByTimeWindow(pedFilter, pedData, pedWindows)
      res.map( r => (r._1, r._2._1, r._2._2, r._2._3, r._2._4, r._2._5, r._2._6)).toVector.sortBy(_._1).writeToCSV(config.getString("output.output_prefix") + "-temporal-tt.csv")
      println(res.map(_._2._2).stats)
    })
  }*/

  // ******************************************************************************************
  //                                  Processing for TRANS-FORM
  // ******************************************************************************************

  if (config.getBoolean("output.write_tt_4_transform")) {
    results.flatten(_._1).computeTT4TRANSFORM(0.0.to(100.0).by(config.getDouble("output.write_tt_4_transform_quantile_interval")), simulationStartTime, simulationEndTime, config.getString("output.write_tt_4_transform_file_name"))
  }
}
