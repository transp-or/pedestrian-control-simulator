import breeze.linalg.DenseVector
import breeze.numerics.pow
import com.typesafe.config.{Config, ConfigFactory}
import hubmodel.DES.SFGraphSimulator
import hubmodel._
import hubmodel.demand.{PedestrianFlows, ReadDisaggDemand, TimeTable}
import hubmodel.mgmt.ControlDevices
import hubmodel.output.TRANSFORM.PopulationProcessing
import hubmodel.output.image.{DrawGraph, DrawWalls, DrawWallsAndGraph}
import hubmodel.output.video.MovingPedestriansWithDensityWithWallVideo
import hubmodel.ped.PedestrianSim
import hubmodel.supply.graph.{BinaryGate, GraphReader}
import hubmodel.supply.continuous.{ContinuousSpaceReader, Wall}
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
  val infraSF = new ContinuousSpaceReader(config.getString("files.walls"))

  // Builds the graph used for route choice. This Graph is coposed of multiple different link types.
  val infraGraph = new GraphReader(
    config.getString("files.graph"),
    config.getBoolean("sim.use_flow_gates"),
    config.getBoolean("sim.use_binary_gates"),
    config.getBoolean("sim.use_amw"),
    config.getBoolean("sim.use_flow_sep"),
    config.getBoolean("sim.measure_density")
  )

  // Collects the control devices which have been read by the graph parser
  val controlDevices = new ControlDevices(infraGraph)

  // Loads the train time table used to create demand from trains
  val timeTable = new TimeTable(config.getString("files.timetable"))

  // Loads the pedestrian flows. These are either exogenous to the trains (from outside) or flows originating from trains.
  val flows =  new PedestrianFlows(config.getString("files.flows"), timeTable, config.getBoolean("sim.use_flows"))

  // Loads the disaggregate pedestrian demand.
  val disaggPopulation = if (config.getBoolean("sim.use_disaggregate_demand")) {
    new ReadDisaggDemand(config.getString("files.disaggregate_demand"))
  } else {
    null
  }

  // Creates images representing the walls, route graph and both overlaid. Used to visualize the input data.
  val wallsImage = new DrawWalls(infraSF.continuousSpace.walls, config.getString("output.output_prefix") + "_wallsWithNames.png", showNames = true)
  val graphImage = new DrawGraph(infraGraph.graph.standardEdges.map(e => (e.startVertex, e.endVertex)).toVector, config.getString("output.output_prefix") + "_graph.png")
  val fullImage = new DrawWallsAndGraph(infraSF.continuousSpace.walls, infraGraph.graph.standardEdges.map(e => (e.startVertex, e.endVertex)).toVector, config.getString("output.output_prefix") + "_wallsAndGraph.png")

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
      graph = infraGraph.graph,
      timeTable = timeTable,
      pedestrianFlows = flows,
      controlDevices = controlDevices
    )
  }

  /** Creates, runs and makes a video from the simulation. No results are processed.
    * Making the video can take some time.
    */
  def runSimulationWithVideo(): ResultsContainer = {

    // create simulation
    val sim = createSimulation()

    if (config.getBoolean("sim.use_disaggregate_demand")) {
      sim.insertMultiplePedestrians(disaggPopulation.pedestrians)
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

    new MovingPedestriansWithDensityWithWallVideo(
      config.getString("output.output_prefix") + "_moving_pedestrians_walls.mp4",
      sim.spaceSF.walls,
      math.max((1.0 / config.getDouble("output.video_dt")).toInt, 1),
      sim.populationCompleted ++ sim.population,
      sim.criticalArea,
      gates.map(g => g.ID -> g).toMap,
      sim.gatesHistory.map(p => ((p._1.value*1).round.toInt, p._2)),
      sim.densityHistory.map(p => ((p._1.value*1).round.toInt, p._2)),
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
    if (config.getBoolean("sim.use_disaggregate_demand")) {
      simulator.insertMultiplePedestrians(disaggPopulation.pedestrians)
    }
    timeBlock(simulator.run())
    collectResults(simulator)
  }

  // Runs the simulations in parallel or sequential based on the config file.
  val results: Vector[ResultsContainer] = {
    if (config.getBoolean("output.make_video") && n == 0){
      Vector(runSimulationWithVideo())
    }
    else if (config.getBoolean("output.make_video") && n > 0 && runSimulationsInParallel) {
      val simulationCollection: collection.parallel.ParSeq[SFGraphSimulator] = collection.parallel.ParSeq.fill(n-1)(createSimulation())
      simulationCollection.tasksupport = new ForkJoinTaskSupport(new java.util.concurrent.ForkJoinPool(config.getInt("execution.threads")))
      simulationCollection.par.map(runAndCollect).seq.toVector :+ runSimulationWithVideo()
    }
    else if (config.getBoolean("output.make_video") && n > 0 && !runSimulationsInParallel) {
      val simulationCollection: collection.immutable.Vector[SFGraphSimulator] = collection.immutable.Vector.fill(n-1)(createSimulation())
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


    def flattenTuple[T: Numeric](t: (String, String, (Int, Double, Double, Double, T, T))): (String, String, Int, Double, Double, Double, T, T)  = (t._1, t._2, t._3._1, t._3._2, t._3._3, t._3._4, t._3._5, t._3._6)

    // Computes the tt stats per OD passed in config file
    val ODPairsToAnalyse: Iterable[(String, String)] = config.getStringList("results-analysis.o_nodes").asScala.zip(config.getStringList("results-analysis.d_nodes").asScala).map(t => (t._1, t._2))
    val TTOD: Iterable[(Time, Vertex, Vertex)] = results.flatMap(r => r._1.map(p => (p.travelTime, p.origin, p.finalDestination)))
    (for (od <- ODPairsToAnalyse) yield {
      flattenTuple(od._1, od._2, TTOD.filter(tod => tod._2.nameCompare(od._1) && tod._3.nameCompare(od._2)).map(_._1.value).stats)
    }).toVector.writeToCSV(config.getString("output.output_prefix") + "_travel_times_OD_stats.csv", columnNames=Some(Vector("O", "D", "size", "mean", "variance", "median", "min", "max")), rowNames=None)


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
    println("Writing VS to file")
    writePopulationTrajectories(results.head._1, config.getString("output.output_prefix") + "_simulation_trajectories.csv")
  }

  // ******************************************************************************************
  //                                  Processing for TRANS-FORM
  // ******************************************************************************************

  if (config.getBoolean("output.write_tt_4_transform")) {
    results.flatten(_._1).computeTT4TRANSFORM(0.0.to(100.0).by(config.getDouble("output.write_tt_4_transform_quantile_interval")), simulationStartTime, simulationEndTime, config.getString("output.output_prefix") + "_" + config.getString("files.ped_walking_time_dist"))
  }
}
