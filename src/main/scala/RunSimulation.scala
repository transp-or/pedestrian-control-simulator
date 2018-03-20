import breeze.linalg.DenseVector
import breeze.numerics.pow
import com.typesafe.config.{Config, ConfigFactory}
import hubmodel.demand.{PedestrianFlows, ReadDisaggDemand, TimeTable}
import hubmodel.output.TRANSFORM.PopulationProcessing
import hubmodel.output.image.{DrawGraph, DrawWalls, DrawWallsAndGraph}
import hubmodel.output.video.MovingPedestriansWithDensityWithWallVideo
import hubmodel.supply.{BinaryGate, ContinuousSpaceReader, ControlDevices, GraphReader}
import hubmodel._
import myscala.timeBlock
import myscala.math.stats.{ComputeStats, stats}
import myscala.output.SeqOfSeqExtensions.SeqOfSeqWriter
import myscala.output.SeqTuplesExtensions.SeqTuplesWriter

import scala.collection.parallel.ForkJoinTaskSupport
import scala.collection.JavaConverters._

/**
  * Runs the simulations based on the configuration file. This configuration file contains all the details regarding
  * the eventual management strategies to use, the number of runs to perform, the simulation time steps and the
  * processing of the results. The keys steps for beaing able to run a simulation are the following:
  *
  * = Definition of the supply =
  * The infrastructure must be specified in two files, the first contains the collection of walls and the second
  * contains the specification of the graph used for route choice with the management strategy specifications.
  *
  * == Wall specification file ==
  * A wall is described as a line. Each wall also contains a comment field, which is only used for debugging and for
  * plotting. The last property of each wall is the "type" field, indicating whether the wall is part of the outer
  * shell or not. This leads to the following:
  *   - comment: humand-readible information, used for debugging
  *   - x1: x coordinate of start of wall
  *   - y1: y coordinate of start of wall
  *   - x2: x coordinate of end of wall
  *   - y2: y coordinate of end of wall
  *   - type: indicator whethe the wall belongs to the outer shell or not (0 = true, 1 = false)
  *
  * '''Example'''
  * Two other elements are passed in the JSON file: the ''location'' and and ''sublocation'' fields. These are used
  * mainly for readibility reasons. Below is a full example:
  * {{{
  * {
  *   "location": "lausanne",
  *   "sublocation": "PIW",
  *   "walls": [
  *   {
  *     "comment": "pl56-W",
  *     "x1": 36.18,
  *     "y1": 10.72,
  *     "x2": 36.18,
  *     "y2": 2.68,
  *     "type": 0
  *     }, {
  *     "comment": "pl56-W",
  *     "x1": 36.18,
  *     "y1": 2.68,
  *     "x2": 39.865,
  *     "y2": 2.68,
  *     "type": 0
  *     }, {
  *     "comment": "pl56-W",
  *     "x1": 39.865,
  *     "y1": 2.68,
  *     "x2": 39.865,
  *     "y2": 10.72,
  *     "type": 0
  *     }
  *   ]
  * }
  * }}}
  *
  * ==Graph specification file==
  * The graph is composed of two collections. The first is the collection of zones, and the second is the collection
  * of connections between the zones. Alongside the graph specification, the possible management strategies are also
  * defined. These can be empty if no management strategy is passed. The zones are defined as follows:
  *   - name: unique name of the zone
  *   - x: x-coord of center of the zone (obsolete)
  *   - y: y-coord of center of the zone (obsolete)
  *   - x1: x-coord of bottom left
  *   - y1: y-coord of bottom left
  *   - x2: x-coord of bottom right
  *   - y2: y-coord of bottom right
  *   - x3: x-coord of top right
  *   - y3: y-coord of top right
  *   - x4: x-coord of top left
  *   - y4: y-coord of top left
  * The order of these corners __MUST__ be respected.
  *
  * The connectivity specification is:
  *   - node: name of the current node
  *   - connectivity: connections from the current node to the nodes listed here (directed)
  * The connections are directed, hence there must be as many connection objects as nodes. In this context, the terms
  * "node" and "zone" can be used in an interchangable manner.
  *
  * ''' Example'''
  * As for the walls file the ''location'' and and ''sublocation'' fields must exist. They are not used by the
  * simulation but must still be present.
  * {{{
  * {
  *   "location": "lausanne",
  *   "sublocation": "test",
  *   "nodes": [
  *     {
  *       "name": "a",
  *       "x": 0.0,
  *       "y": 0.0,
  *       "x1": 47.71234866828081,
  *       "y1": 247.8312348668281,
  *       "x2": 47.71234866828078,
  *       "y2": 188.6055690072639,
  *       "x3": 113.466828087167,
  *       "y3": 188.6055690072639,
  *       "x4": 113.466828087167,
  *       "y4": 247.8312348668281
  *     }, {
  *       "name": "b",
  *       "x": 0.0,
  *       "y": 0.0,
  *       "x1": 334.5138014527845,
  *       "y1": 169.9518159806295,
  *       "x2": 415.1912832929783,
  *       "y2": 169.9518159806295,
  *       "x3": 415.1912832929783,
  *       "y3": 238.9707021791768,
  *       "x4": 334.5138014527845,
  *       "y4": 238.9707021791768
  *     }, {
  *       "name": "c",
  *       "x": 0.0,
  *       "y": 0.0,
  *       "x1": 239.2021395489995,
  *       "y1": 154.7129236207632,
  *       "x2": 273.3027061212375,
  *       "y2": 121.4648712128312,
  *       "x3": 317.6334426651468,
  *       "y3": 132.5475553488085,
  *       "x4": 294.6155602288862,
  *       "y4": 168.3531502496584
  *     }
  *   ],
  *   "connectivity": [
  *     {
  *       "node": "c",
  *       "connected_to": ["b"]
  *     }, {
  *       "node": "a",
  *       "connected_to": ["b", "c"]
  *     }, {
  *       "node": "b",
  *       "connected_to": ["a"]
  *     }
  *   ],
  *   "flow_gates": [],
  *   "controlled_areas": [],
  *   "binary_gates": [],
  *   "flow_separators": []
  * }
  * }}}
  *
  * = Definition of the demand =
  * As the objective is to simulate transportation hubs (excluding airports) the pedestrian demand can come from two
  * distinct origins: public transport vehicles and walking pedestrians. This data is passed as JSON files to the
  * simulator.
  *
  * == Timetable specification ==
  * The arrival time, departure time, platform and train specifictions are provided in the timetable file. The fields
  * which must be completed are the following:
  *   - id: unique identifier of the vehicle
  *   - type: type of the vehicle
  *   - track: track/platform/shelter where the vehicle arrives
  *   - arrival-time: arrival time of the vehicle (time of day)
  *   - departure-time: departure time of the vehicle (time of day)
  *   - capacity: maximum capacity of the vehicle
  *
  * Although the terminolgy refers to train, any type of public transport vehicle can be used. Buses, trams and trains
  * can be freely combined. The "type" field can be used to identify classes of vehicles. A second element must be
  * included in the timetable specification file: the track to zone mapping. This "map" links the platform to a
  * set of zones where the passengers will disembark/embark.
  *
  * ''' Example'''
  *{{{
  *
"location": "lausanne",
  "trains": [
    {
      "id": "12217",
      "type": "S21",
      "track": 3,
      "arrival-time": "07:05:00",
      "departure-time": "07:07:00",
      "capacity": 515
    }, {
      "id": "12218",
      "type": "S2",
      "track": 4,
      "arrival-time": "07:06:00",
      "departure-time": "07:08:00",
      "capacity": 517
    }
  ],
  "track2nodes": [
    {
      "track": 4,
      "nodes": ["11", "12"]
    }, {
      "track": 3,
      "nodes": ["9", "10"]
    }
  ]
}
  *}}}
  *
  * == Pedestrian flow specification ==
  * The pedestrian flows between the different public tranports vehicles and places in the transportation hub
  * can be specified int wo ways, which can be freely combined. The first is a flow-based specification and the second
  * is a disaggregate approach.
  * === Flow-based specification ===
  * There are two types of flows: flows originating from public transport vehicles and flows originating from a "fixed
  * location". When passengers disembark from a vehicle, they immediately move towards their destination. Hence the
  * time at which they enter the system depends on the arrival time of the vehicle inside the hub. The fields whch
  * define these flows are the following:
  *   - origin: id of the originating vehicle
  *   - destination: id of the destination node or vehicle
  *   - flow: number of pedestrians walking this trip
  *
  * Pedestrians arriving from a "fixed" location do not depend on some sort of scheduled transportation system, hence
  * they arrive independently from one another. They tend to follow a Poisosn process. The requeried fields are:
  *    - origin: origin node of the flow
  *    - destination: destination node or vehicle of the flow
  *    - start: start time of the flow
  *    - end: end time of the flow
  *    - flow: number of pedestrians to be generated in the interval
  *
  * '''Example'''
  * Below a sample file is available:
  * {{{
  * {
  *   "location": "lausanne",
  *   "PTflows": [
  *     {
  *       "origin": "T_12217",
  *       "destination": "T_12218",
  *       "flow": 80
  *     }, {
  *       "origin": "T_12217",
  *       "destination": "S_13",
  *       "flow": 120
  *     }, {
  *       "origin": "T_12217",
  *       "destination": "S_14",
  *       "flow": 100
  *     }
  *   ],
  *   "flows": [
  *     {
  *       "origin": "1",
  *       "destination": "14",
  *       "start": "07:00:00",
  *       "end": "08:00:00",
  *       "flow": 180
  *     }, {
  *       "origin": "1",
  *       "destination": "13",
  *       "start": "07:00:00",
  *       "end": "08:00:00",
  *       "flow": 180
  *     }
  *   ]
  * }
  * }}}
  *
  * === Disaggregate demand ===
  * The disaggregate pedestrian input can also be used. This has been specifically developped for transfering data
  * between the urban model and hub model in the context of the TRANS-FORM project. This data is basically a disaggregate
  * OD matrix. Each entry corresponds to a pedestrian, and the fields to be filled are the following:
  *   - ID: unique ID of the pedestrian
  *   - O: origin zone of the pedestrian
  *   - D: destintation zone of the pedestrian
  *   - entryTime: time at which the pedestrian will enter the simulation environment
  *
  * '''Example'''
  * The data is stored using JSON. Below is a sample. In this sample the exitTime is provided, but this is not required
  * for running the simulations.
  * {{{
  * [
  *   { "ID":"1", "O":"1", "D":"6", "entryTime": 25200.336, "exitTime":25223.771},
  *   { "ID":"3", "O":"7", "D":"6", "entryTime": 25201.366, "exitTime":25271.026},
  *   { "ID":"5", "O":"14", "D":"8", "entryTime": 25201.559, "exitTime":25241.09},
  *   { "ID":"6", "O":"13", "D":"8", "entryTime": 25201.595, "exitTime":25270.086},
  *   { "ID":"7", "O":"12", "D":"8", "entryTime": 25201.863, "exitTime":25249.366},
  *   { "ID":"9", "O":"12", "D":"8", "entryTime": 25205.135, "exitTime":25255.47},
  *   { "ID":"10", "O":"12", "D":"6", "entryTime": 25205.565, "exitTime":25275.628},
  *   { "ID":"11", "O":"12", "D":"7", "entryTime": 25207.035, "exitTime":25246.846}
  * ]
  * }}}
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
  val simulationStartTime: NewTime = NewTime(config.getDouble("sim.start_time"))
  val simulationEndTime: NewTime = NewTime(config.getDouble("sim.end_time"))
  val socialForceInterval: NewTime = NewTime(config.getDouble("sim.sf_dt"))
  val evaluationInterval: NewTime = NewTime(config.getDouble("sim.evaluate_dt"))
  val rebuildTreeInterval: NewTime = NewTime(config.getDouble("sim.rebuild_tree_dt"))

  // number of simulations to run
  val n: Int = config.getInt("sim.nb_runs")

  val runSimulationsInParallel: Boolean = config.getBoolean("execution.parallel")

  // ******************************************************************************************
  //                    Runs the simulation, creates video and outputs results
  // ******************************************************************************************

  println("Running " + n + " simulations")

  // Container for the results from a simulation. This type chould be modified if the collectResults function is modified
  type ResultsContainer = (Vector[PedestrianSim], List[(NewTime, Double)], List[(NewTime, Double)])

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
      (simulationStartTime.value to simulationEndTime.value by config.getDouble("output.video_dt")).map(new NewTime(_))
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
    val densityTimes: Vector[NewTime] = results.head._2.unzip._1.toVector
    val inflowTimes: Vector[NewTime] = results.head._3.unzip._1.toVector

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
      .flatMap(r => r._1._1.map(p => (r._2, p.travelTime, p.oZone, p.dZone)))
      .writeToCSV(
        config.getString("output.output_prefix") + "_travel_times_OD.csv",
        columnNames = Some(Vector("run", "travel_time", "origin_id", "destination_id")),
        rowNames = None
      )


    def flattenTuple[T: Numeric](t: (String, String, (Int, Double, Double, Double, T, T))): (String, String, Int, Double, Double, Double, T, T)  = (t._1, t._2, t._3._1, t._3._2, t._3._3, t._3._4, t._3._5, t._3._6)

    // Computes the tt stats per OD passed in config file
    val ODPairsToAnalyse: Iterable[(String, String)] = config.getStringList("results-analysis.o_nodes").asScala.zip(config.getStringList("results-analysis.d_nodes").asScala).map(t => (t._1, t._2))
    val TTOD: Iterable[(NewTime, VertexRectangle, VertexRectangle)] = results.flatMap(r => r._1.map(p => (p.travelTime, p.oZone, p.dZone)))
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
    if (config.getBoolean("output.write_tt_stats")) results.map(r => stats(r._1.map(p => p.travelTime.value))).writeToCSV(config.getString("output.output_prefix") + "_travel_times_stats.csv", rowNames = None, columnNames = Some(Vector("mean", "variance", "median", "max", "min")))

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
