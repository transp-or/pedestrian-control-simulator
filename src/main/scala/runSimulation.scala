
import java.util.concurrent.Executors

import breeze.linalg.DenseVector
import breeze.numerics.{pow, sqrt}
import hubmodel.{PedestrianDES, Position, SFGraphSimulator, Time}
import hubmodel.input.demand.{PedestrianFlows, TimeTable}
import hubmodel.input.infrastructure.{BinaryGate, ContinuousSpaceReader, GraphReader, NameConversions, NodeNaming, ODInfrastructureParser, SocialForceSpace}
import hubmodel.output._
import com.typesafe.config.ConfigFactory
import myscala.output.SeqOfSeqExtensions.SeqOfSeqWriter
import myscala.output.SeqTuplesExtensions.SeqTuplesWriter
import myscala.math.stats.stats

import scala.concurrent.{ExecutionContext, Future}

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

  // Builds the graph used for route choice. This Graph is coposed of multiple different link types.
  val infraGraph = new GraphReader(config.getString("files.graph"))

  // Builds the set of walls used by the Social force model
  val infraSF = new ContinuousSpaceReader(config.getString("files.walls"))

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


  // number of simulations to run
  val n: Int = config.getInt("sim.nb_runs")

  // ******************************************************************************************
  //                    Runs the simulation, creates video and outputs results
  // ******************************************************************************************

  //implicit val ec = ExecutionContext.fromExecutorService(Executors.newWorkStealingPool(4))
  // Runs the simulations
  //val densityCollection: IndexedSeq[Future[(scala.collection.mutable.ArrayBuffer[(Time, Double)], Vector[Time], Vector[Time], collection.mutable.ArrayBuffer[(Time, Double)])]] = for (i <- 0 until n) yield {
  val outputData: Vector[(scala.collection.mutable.ArrayBuffer[(Time, Double)], Vector[Time], Vector[Time], collection.mutable.ArrayBuffer[(Time, Double)])] = (for (i <- 0 until n) yield {

    // Creates one instance of the simulation
    val sim = new SFGraphSimulator(
      startTime = simulationStartTime,
      finalTime = simulationEndTime,
      sf_dt = socialForceInterval, // might impact parameters of SFM
      evaluate_dt = 1.0,
      spaceSF = infraSF.continuousSpace,
      graph = infraGraph.graph,
      timeTable,
      flows,
      nodeNaming = nameMapping
    )

    // Executes the simulation
    sim.run()

    // Creates a video (moving pedestrians) of the simulation. This is done only once and only if the
    // corresponding parameter is set to true.
    if (i == 0 && config.getBoolean("output.make_video")) {

      // TODO not needed, everything is passed with the graph file
      val gates: List[BinaryGate] = List()

      new MovingPedestriansWithDensityVideo(
        config.getString("output.output_prefix") + "_moving_pedestrians.mp4",
        Option(config.getString("output.bckg_image_video")),
        (config.getDouble("output.bckg_image_width"), config.getDouble("output.bckg_image_height")),
        (1.0 / 0.1).toInt,
        sim.populationCompleted ++ sim.population,
        sim.criticalArea.head,
        gates.map(g => g.ID -> g).toMap,
        sim.gatesHistory.map(p => ((p._1 * 1000).round.toInt, p._2)),
        sim.densityHistory.map(p => ((p._1 * 1000).round.toInt, p._2)),
        (simulationStartTime * 1000).round.toInt to (simulationEndTime * 1000).round.toInt by 100)
    }

    sim.populationCompleted.filter(p => Vector(9, 10).contains(p.oZone)).map(_.travelTime)

    // outputs for each simulation: (this should be changed based on the requirements
    // - the density evolution in the specified zone
    // - the travel times of pedestrians who completed their trips
    (
      sim.densityHistory,
      sim.populationCompleted.map(_.travelTime),
      sim.populationCompleted.filter(p => Vector(9, 10).contains(p.oZone)).map(_.travelTime),
      sim.inflowHistory
    )
  }).toVector

  //val agg: Future[Seq[(scala.collection.mutable.ArrayBuffer[(Time, Double)], Vector[Time], Vector[Time], collection.mutable.ArrayBuffer[(Time, Double)])], Vector[Int])] = Future.sequence(densityCollection)
  //val densityCollection2 = Await.result(agg, Duration(200000, "seconds"))

  // ******************************************************************************************
  //                                Writes results to CSV
  // ******************************************************************************************

  // Collects times at which densitiey where measured
  val densityTimes: Vector[Time] = outputData.head._1.unzip._1.toVector

  val meanDensity: DenseVector[Double] = outputData.map(res => DenseVector(res._1.unzip._2.toArray)).foldLeft(DenseVector.fill(outputData.head._1.size)(0.0)){(old: DenseVector[Double], n: DenseVector[Double]) => old+(n/outputData.size.toDouble)}
  val varianceDensity: DenseVector[Double] = sqrt(outputData.map(res => DenseVector(res._1.unzip._2.toArray)).foldLeft(DenseVector.fill(outputData.head._1.size)(0.0)){(old: DenseVector[Double], n: DenseVector[Double]) => old+(pow(n-meanDensity,2)/(outputData.size.toDouble-1.0))})

  // Writes some results to csv files
  if (config.getBoolean("output.write_travel_times")) outputData.map(_._2).writeToCSV(config.getString("output.output_prefix") + "_travel_times.csv")
  if (config.getBoolean("output.write_densities")) (densityTimes +: meanDensity.toScalaVector() +: varianceDensity.toScalaVector() +: outputData.map(_._1.map(_._2).toVector) ).writeToCSV(
    config.getString("output.output_prefix") + "_densities.csv",
    rowNames = None,
    columnNames = Some(Vector("time", "mean", "variance") ++ Vector.fill(n)("r").zipWithIndex.map(t => t._1+t._2.toString))
  )
  if (config.getBoolean("output.write_tt_stats")) outputData.map(d => stats(d._2)).writeToCSV(config.getString("output.output_prefix") + "_travel_time_stats.csv", rowNames = None, columnNames = Some(Vector("mean", "variance", "median", "max", "min")))
  if (config.getBoolean("output.write_tt_stats")) outputData.map(d => stats(d._3)).writeToCSV(config.getString("output.output_prefix") + "_travel_time_o_34_stats.csv", rowNames = None, columnNames = Some(Vector("mean", "variance", "median", "max", "min")))
  (densityTimes +: outputData.map(d => d._4.map(_._2).toVector)).writeToCSV(config.getString("output.output_prefix") + "_inflow_value.csv")
}

