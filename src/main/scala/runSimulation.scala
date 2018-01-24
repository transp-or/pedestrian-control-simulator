
import breeze.linalg.DenseVector
import breeze.numerics.{pow, sqrt}
import com.typesafe.config.ConfigFactory
import hubmodel.input.demand.{PedestrianFlows, TimeTable}
import hubmodel.input.infrastructure.{BinaryGate, ContinuousSpaceReader, GraphReader, NodeNaming, ReadControlDevices}
import hubmodel.output._
import hubmodel.{PedestrianSim, SFGraphSimulator, Time, timeBlock}
import myscala.math.stats.stats
import myscala.output.SeqOfSeqExtensions.SeqOfSeqWriter
import myscala.output.SeqTuplesExtensions.SeqTuplesWriter
import myscala.output.SeqExtension.SeqWriter

import scala.collection.parallel.ForkJoinTaskSupport

/** main class for running simulations of the train station.
  *
  */
object runSimulation extends App {

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
  val config = ConfigFactory.load(confFile)

  // checkValid(), just as in the plain SimpleLibContext.
  // Note that these fields are NOT lazy, because if we're going to
  // get any exceptions, we want to get them on startup.
  config.checkValid(ConfigFactory.defaultReference())

  // ******************************************************************************************
  //                        Loads the input data
  // ******************************************************************************************

  println("Reading and creating initial data")

  // Builds the graph used for route choice. This Graph is coposed of multiple different link types.
  val infraGraph = new GraphReader(config.getString("files.graph"))

  // Builds the set of walls used by the Social force model
  val infraSF = new ContinuousSpaceReader(config.getString("files.walls"))

  // Reads the elements linked to control strategies. They are stored in the same file as the graph
  val controlDevices = new ReadControlDevices(config.getString("files.graph"), infraGraph.graph.vertexMap)

  // Loads the train time table used to create demand from trains
  val timeTable = new TimeTable(config.getString("files.timetable"))

  // Loads the pedestrian flows. These are either exogenous to the trains (from outside) or flows originating from trains.
  val flows = new PedestrianFlows(config.getString("files.flows"), timeTable)

  // mapping from strings to ints and vice-versa for nodes
  // TODO this should not be required. When objects are generated, they should have a unique ID.
  val nameMapping = new NodeNaming(config.getString("files.name_map"))

  // Creates images representing the walls, route graph and both overlaid. Used to visualize the input data.
  val wallsImage = new DrawWalls(infraSF.continuousSpace.walls, config.getString("output.output_prefix") + "_wallsWithNames.png", showNames = true)
  val graphImage = new DrawGraph(infraGraph.graph.standardEdges.map(e => (e.startVertex, e.endVertex)).toVector, config.getString("output.output_prefix") + "_graph.png")
  val fullImage = new DrawWallsAndGraph(infraSF.continuousSpace.walls, infraGraph.graph.standardEdges.map(e => (e.startVertex, e.endVertex)).toVector, config.getString("output.output_prefix") + "_wallsAndGraph.png")

  // Loads the start time, end time and time intervals
  val simulationStartTime: Time = config.getDouble("sim.start_time")
  val simulationEndTime: Time = config.getDouble("sim.end_time")
  val socialForceInterval: Time = config.getDouble("sim.sf_dt")
  val evaluationInterval: Time = config.getDouble("sim.evaluate_dt")

  // number of simulations to run
  val n: Int = config.getInt("sim.nb_runs")

  val runSimulationsInParallel: Boolean = config.getBoolean("execution.parallel")

  // ******************************************************************************************
  //                    Runs the simulation, creates video and outputs results
  // ******************************************************************************************

  println("Running simulations")

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
      sf_dt = socialForceInterval, // might impact parameters of SFM
      evaluate_dt = evaluationInterval,
      spaceSF = infraSF.continuousSpace,
      graph = infraGraph.graph,
      timeTable = timeTable,
      pedestrianFlows = flows,
      nodeNaming = nameMapping,
      controlDevices = controlDevices
    )
  }

  /** Creates, runs and makes a video from the simulation. No results are processed.
    * Making the video can take some time.
    */
  def runSimulationWithVideo(): Unit = {

    println("Making video of simulation, this can take some time...")

    // create simulation
    val sim = createSimulation()

    // execute simulation
    sim.run()

    val gates: List[BinaryGate] = List()

    new MovingPedestriansWithDensityVideo(
      config.getString("output.output_prefix") + "_moving_pedestrians.mp4",
      if (config.getString("output.bckg_image_video") == "None") {
        None
      } else {
        Option(config.getString("output.bckg_image_video"))
      },
      (config.getDouble("output.bckg_image_width"), config.getDouble("output.bckg_image_height")),
      (1.0 / socialForceInterval).toInt,
      sim.populationCompleted ++ sim.population,
      sim.criticalArea,
      gates.map(g => g.ID -> g).toMap,
      sim.gatesHistory.map(p => ((p._1 * 1000).round.toInt, p._2)),
      sim.densityHistory.map(p => ((p._1 * 1000).round.toInt, p._2)),
      (simulationStartTime * 1000).round.toInt to (simulationEndTime * 1000).round.toInt by 100)


    if (config.getBoolean("output.write_trajectories_as_VS")) {
      println("Writing VS data from video")
      (sim.population ++ sim.populationCompleted).map(p => p.toVisioSafeFormat()).writeToCSV(config.getString("output.output_prefix")+"_simulation_trajectories.csv")
    }
  }

  /** Runs the simulation and then collects the results. The simulation is timed.
    *
    * @param simulator simulation to run
    * @return results collected from the simulation
    */
  def runAndCollect(simulator: SFGraphSimulator): ResultsContainer = {
    timeBlock(simulator.run())
    collectResults(simulator)
  }

  // Runs the simulations in parallel or sequential based on the config file.
  val results: Vector[ResultsContainer] = {
    if (runSimulationsInParallel) {
      val simulationCollection: collection.parallel.ParSeq[SFGraphSimulator] = collection.parallel.ParSeq.fill(n)(createSimulation())
      simulationCollection.tasksupport = new ForkJoinTaskSupport(new java.util.concurrent.ForkJoinPool(config.getInt("execution.threads")))
      simulationCollection.par.map(runAndCollect).seq.toVector
    }
    else {
      val simulationCollection: collection.immutable.Vector[SFGraphSimulator] = collection.immutable.Vector.fill(n)(createSimulation())
      simulationCollection.map(runAndCollect)
    }
  }

  // if specified in the config file, makes a video from the simulation
  if (config.getBoolean("output.make_video")) {
    runSimulationWithVideo()
  }

  // ******************************************************************************************
  //                           Processes and writes results to CSV
  // ******************************************************************************************

  println("Processing results")

  // Collects times at which densities where measured
  val densityTimes: Vector[Time] = results.head._2.unzip._1.toVector
  val inflowTimes: Vector[Time] = results.head._3.unzip._1.toVector

  // compute mean and variance of density
  val meanDensity: DenseVector[Double] = results.map(res => DenseVector(res._2.unzip._2.toArray)).foldLeft(DenseVector.fill(results.head._2.size)(0.0)) { (old: DenseVector[Double], n: DenseVector[Double]) => old + (n / results.size.toDouble) }
  val varianceDensity: DenseVector[Double] = sqrt(results.map(res => DenseVector(res._2.unzip._2.toArray)).foldLeft(DenseVector.fill(results.head._2.size)(0.0)) { (old: DenseVector[Double], n: DenseVector[Double]) => old + (pow(n - meanDensity, 2) / (results.size.toDouble - 1.0)) })

  // Collects then writes individual travel times to csv
  if (config.getBoolean("output.write_travel_times")) results.map(r => r._1.map(p => p.travelTime)).writeToCSV(config.getString("output.output_prefix") + "_travel_times.csv")

  // writes densities to csv, first column is time, second column is mean, third column is var, then all individual densities
  if (config.getBoolean("output.write_densities")) (densityTimes +: meanDensity.toScalaVector() +: varianceDensity.toScalaVector() +: results.map(_._2.map(_._2).toVector)).writeToCSV(
    config.getString("output.output_prefix") + "_densities.csv",
    rowNames = None,
    columnNames = Some(Vector("time", "mean", "variance") ++ Vector.fill(n)("r").zipWithIndex.map(t => t._1 + t._2.toString))
  )

  // computes statistics on travel times and writes them
  if (config.getBoolean("output.write_tt_stats")) results.map(r => stats(r._1.map(p => p.travelTime))).writeToCSV(config.getString("output.output_prefix") + "_travel_times_stats.csv", rowNames = None, columnNames = Some(Vector("mean", "variance", "median", "max", "min")))

  // collects and writes inflow values to csv
  if (config.getBoolean("output.write_inflow")) (inflowTimes +: results.map(d => d._3.map(_._2).toVector)).writeToCSV(config.getString("output.output_prefix") + "_inflow.csv")

  // writes the pedestrian tajectories in the same format as VS for analysing the data
  if ( !config.getBoolean("output.make_video") && config.getBoolean("output.write_trajectories_as_VS")) { results.head._1.map(p => p.toVisioSafeFormat()).writeToCSV(config.getString("output.output_prefix")+"_simulation_trajectories.csv") }
}

