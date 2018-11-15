import java.io.{BufferedWriter, File, FileWriter}
import java.math.MathContext
import java.nio.file.{Files, Paths}

import com.typesafe.config.{Config, ConfigFactory}
import hubmodel.DES.NOMADGraphSimulator
import hubmodel.demand.{CreatePedestrian, PedestrianFlowFunction_New, PedestrianFlowPT_New, PedestrianFlow_New, ProcessTimeTable, PublicTransportSchedule, readDisaggDemand, readDisaggDemandTF, readPedestrianFlows, readSchedule, readScheduleTF}
import hubmodel.output.image.{DrawControlDevicesAndWalls, DrawGraph, DrawWalls, DrawWallsAndGraph}
import hubmodel.output.video.MovingPedestriansWithDensityWithWallVideo
import hubmodel.ped.{PedestrianNOMAD, PedestrianNOMADWithGraph, PedestrianSim, PedestrianTrait}
import hubmodel.supply.{NodeID_New, NodeParent, StopID_New, TrainID_New}
import hubmodel.supply.continuous.{MovableWall, ReadContinuousSpace}
import hubmodel.supply.graph._
import hubmodel.tools.cells.{DensityMeasuredArea, Rectangle}
import myscala.math.vector.{Vector2D, Vector3D}
import myscala.output.SeqOfSeqExtensions.SeqOfSeqWriter
import myscala.output.SeqTuplesExtensions.SeqTuplesWriter
import myscala.timeBlock
import org.apache.commons.lang3.RandomStringUtils
import play.api.libs.json.Reads._
import play.api.libs.json._

import scala.collection.immutable.NumericRange
import scala.math.BigDecimal.RoundingMode
import scala.util.{Failure, Success, Try}
import hubmodel.TimeNumeric.mkOrderingOps
import hubmodel.demand.flows.{ProcessDisaggregatePedestrianFlows, ProcessPedestrianFlows}

import scala.reflect.ClassTag


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


  class Time(val value: BigDecimal) extends AnyVal {

    def +(m: Time): Time = new Time(this.value + m.value)

    def addDouble(m: Double): Time = new Time(this.value + m)

    def -(m: Time): Time = new Time(this.value - m.value)

    def abs: Time = new Time(this.value.abs)

    def *(m: Double) = new Time(this.value * m)

    def asReadable: String = {
      val hours: Int = (value / 3600.0).setScale(0, RoundingMode.CEILING).toInt
      val minutes: Int = ((value - hours * 3600) / 60.0).setScale(0, RoundingMode.CEILING).toInt
      val seconds: Double = (value - hours * 3600 - minutes * 60).setScale(0, RoundingMode.CEILING).toInt
      hours.toString + ":" + minutes.toString + ":" + seconds.toString
    }

    def asVisioSafe: String = {
      val h: Int = (this.value / 3600.0).setScale(0, RoundingMode.FLOOR).toInt
      val min: Int = ((this.value - h * 3600)/60.0).setScale(0, RoundingMode.FLOOR).toInt
      val s: Int = (this.value  - 3600 * h - 60 * min).setScale(0, RoundingMode.FLOOR).toInt
      val ms: Int = (1000 * (this.value - 3600 * h - 60 * min - s)).round(new MathContext(4, java.math.RoundingMode.HALF_UP)).toInt
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



  /** Generation of a pseudo-UUID. This can be used to generate unique identifiers for objects.
    *
    * @return UUID formatted as a String
    */
  def generateUUID: String = RandomStringUtils.randomAlphanumeric(8)// java.util.UUID.randomUUID.toString


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


  def writePopulationTrajectoriesJSON(pop: Iterable[PedestrianSim], file: String, times: NumericRange[BigDecimal]): Unit = {

    // collects the traj data
    val trajDataByTime: Map[Time, Iterable[(Time, Position, String)]] = pop.flatMap(p => p.getHistoryPosition.map(r => (r._1, r._2, p.ID))).groupBy(_._1).filter(t => times.contains(t._1.value))

    val sortedKeys: Vector[Time] = trajDataByTime.keys.toVector.sorted

    // opens the file for writing
    val f = new File(file)
    val bw = new BufferedWriter(new FileWriter(f))
    bw.write("[")
    // write first value separately to deal with commas seperating
    bw.write("{\"time\":" + sortedKeys.head.toString + ",")
    bw.write("\"data\":[" + trajDataByTime(sortedKeys.head).map(d => "{\"id\":\"" + d._3 + "\",\"x\":" + d._2.X.toString + ",\"y\":" + d._2.Y.toString + "}").mkString(","))
    bw.write("]}")
    for (t <- sortedKeys.tail) {
      bw.write(",{\"time\":" + t.toString + ",")
      bw.write("\"data\":[")
      bw.write(trajDataByTime(t).map(d => "{\"id\":\"" + d._3 + "\",\"x\":" + d._2.X.toString + ",\"y\":" + d._2.Y.toString + "}").mkString(","))
      bw.write("]}")
    }

    bw.write("]")
    bw.close()
  }

  def writeODJSON(pop: Iterable[PedestrianSim], ODZones: Iterable[String], file: String): Unit = {
    def buildStringFromIDs(IDs: Iterable[String]): String = {
      if (IDs.isEmpty)  { "" }
      else IDs.mkString("\"", "\", \"", "\"")
    }

    val data: Map[String, (Iterable[String], Iterable[String])] = ODZones.map(od => od -> (pop.filter(_.origin.nameCompare(od)).map(_.ID), pop.filter(_.finalDestination.nameCompare(od)).map(_.ID))).toMap
    val f = new File(file)
    val bw = new BufferedWriter(new FileWriter(f))
    bw.write("{")
    // write first value separately to deal with commas seperating
    bw.write("\"" + data.head._1 + "\": {\"O\": [" + buildStringFromIDs(data.head._2._1) + "], \"D\":[" + buildStringFromIDs(data.head._2._2) + "]}")

    // write other elements
    for (elem <- data.tail) {
      bw.write(",\n\"" + elem._1 + "\": {\"O\": [" + buildStringFromIDs(elem._2._1) + "], \"D\":[" + buildStringFromIDs(elem._2._2) + "]}")
    }
    bw.write("}")
    bw.close()
  }

  //type T <: PedestrianNOMAD

  /** Creates a simulation, but does not run it
    *
    * @return simulator ready to run
    */
  def createSimulation[T <: PedestrianNOMAD](config: Config)(implicit tag: ClassTag[T]): NOMADGraphSimulator[T] = {


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
    val (routeGraph, controlDevices) = readGraph[T](
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
      (
        readSchedule(config.getString("files.timetable")),
        readPTStop2GraphVertexMap(config.getString("files.zones_to_vertices_map"))
      )
    } else if (config.hasPath("files.timetable_TF") && !config.getIsNull("files.timetable_TF")) {
      (
        readScheduleTF(config.getString("files.timetable_TF")),
        readPTStop2GraphVertexMap(config.getString("files.zones_to_vertices_map"))
      )
    } else if (flows._2.isEmpty) {
      println(" * no time table is required as PT induced flows are empty")
      (new PublicTransportSchedule("unused", Vector()), new Stop2Vertex(Map(), Map()))
    } else {
      throw new IllegalArgumentException("both time tables files are set to null in config file")
    }

    // Loads the disaggregate pedestrian demand.
    val disaggPopulation: Iterable[(String, String, Time)] = if (config.hasPath("files.flows_TF") && config.getIsNull("files.flows_TF") && !config.getIsNull("files.disaggregate_demand")) {
      readDisaggDemand(config.getString("files.disaggregate_demand"))
    } else if (config.hasPath("files.disaggregate_demand") && config.getIsNull("files.disaggregate_demand") && !config.getIsNull("files.flows_TF")) {
      readDisaggDemandTF(config.getString("files.flows_TF"))
    } else {
      println(" * using only standard pedestrian flows")
      Iterable()
    }

    /** Takes a conceptual node (train or on foot) and returns the set of "real" nodes (the ones used by the graph)
      * in which the pedestrians must be created.
      *
      * @param conceptualNode node representing the train or the pedestrian zone
      * @return iterable in which the pedestrians will be created
      */
    def conceptualNode2GraphNodes(conceptualNode: NodeParent): Iterable[Rectangle] = {
      conceptualNode match {
        case x: TrainID_New => stop2Vertex.stop2Vertices(timeTable.timeTable(x).stop).map(n => routeGraph.vertexMapNew(n))
        case x: NodeID_New => Iterable(routeGraph.vertexMapNew(x.ID))
        case x: StopID_New => Iterable(routeGraph.vertexMapNew(x.ID.toString))
        case _ => throw new Exception("Track ID should not be there !")
      }
    }

    // Loads the start time, end time and time intervals
    val simulationStartTime: Time = Time(config.getDouble("sim.start_time"))
    val simulationEndTime: Time = Time(config.getDouble("sim.end_time"))
    val socialForceInterval: Time = Time(config.getDouble("sim.sf_dt"))
    val routeUpdateInterval: Time = Time(config.getDouble("sim.route_dt"))
    val evaluationInterval: Time = Time(config.getDouble("sim.evaluate_dt"))
    val rebuildTreeInterval: Time = Time(config.getDouble("sim.rebuild_tree_dt"))

    // number of simulations to run
    val n: Int = config.getInt("sim.nb_runs")



    val sim: NOMADGraphSimulator[T] = new NOMADGraphSimulator[T](
      simulationStartTime,
      simulationEndTime,
      logDir = None,
      sf_dt = socialForceInterval,
      route_dt = routeUpdateInterval,
      evaluate_dt = evaluationInterval,
      rebuildTreeInterval = Some(rebuildTreeInterval),
      spaceMicro = infraSF.continuousSpace,
      graph = routeGraph,
      timeTable = timeTable,
      stop2Vertices = conceptualNode2GraphNodes,
      controlDevices = controlDevices
    )

    if (disaggPopulation.nonEmpty) { sim.insertEventWithZeroDelay(new ProcessDisaggregatePedestrianFlows(disaggPopulation, sim)) }

    val PTInducedFlows = flows._2
    sim.insertEventWithZeroDelay(new ProcessTimeTable[T](timeTable, PTInducedFlows, sim))
    sim.insertEventWithZeroDelay(new ProcessPedestrianFlows[T](flows._1, flows._3, sim))

    sim
  }

  // ******************************************************************************************
  //                    Runs the simulation, creates video and outputs results
  // ******************************************************************************************

  case class ResultsContainerNew(exitCode:Int, completedPeds: Vector[PedestrianSim], uncompletedPeds: Vector[PedestrianSim], densityZones: Map[String, DensityMeasuredArea])

  // Container for the results from a simulation. This type chould be modified if the collectResults function is modified
  case class ResultsContainerRead(tt: Vector[(String, String, Double, Double, Double)], monitoredAreaDensity: Option[(Vector[Double] , Vector[Vector[Double]])], monitoredAreaIndividualDensity: Option[Vector[(BigDecimal, BigDecimal)]])

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
  def collectResults[T <: PedestrianNOMAD](simulator: NOMADGraphSimulator[T]): ResultsContainerNew = {
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

  /**
    * Writes the main results from a simulation to csv files. This is needed when running dozens of simulations as the
    * RAM fills up too quickly otherwise.
    * @param simulator simulator with completed results
    * @param prefix prefix to the file name
    * @param path path where to write the file, default is empty
    */
  def writeResults[T <: PedestrianNOMAD](simulator: NOMADGraphSimulator[T], prefix: String = "", dir: Option[String], writeTrajectoriesVS: Boolean = false, writeTrajectoriesJSON: Boolean = false): Unit = {

    // TODO: check if files exists and remove them if they are inside tmp, and warn about them if they are in output_dir
    val path: String = dir match {
      case Some(str) => str
      case None => {
        if (!Files.exists(Paths.get("tmp/"))) {
          Files.createDirectory(Paths.get("tmp/"))
        } else {
          // delete contents
        }
        "tmp/"
      }
    }

    if (simulator.exitCode == 0) {
      simulator.populationCompleted.map(p => (p.origin.name, p.finalDestination.name, p.travelTime.value, p.entryTime.value, p.exitTime.value)).writeToCSV(prefix + "tt_"+simulator.ID+".csv", path)
      if (simulator.criticalAreas.nonEmpty) {
        (simulator.criticalAreas.head._2.densityHistory.map(_._1.value).toVector +: simulator.criticalAreas.map(_._2.densityHistory.map(_._2).toVector).toVector).writeToCSV(prefix + "density_" + simulator.ID + ".csv", path)
        simulator.criticalAreas.head._2.paxIndividualDensityHistory.flatMap(v => Vector.fill(v._2.size)(v._1.value).zip(v._2)).toVector.writeToCSV(prefix + "individual_densities_" + simulator.ID + ".csv", path)
      }
    }

    if (writeTrajectoriesVS) {
      println("Writing trajectories as VS to file")
      writePopulationTrajectories(simulator.populationCompleted ++ simulator.population, prefix + "_simulation_trajectories_VS_" + simulator.ID + ".csv")
    }

    if (writeTrajectoriesJSON) {
      println("Writing Trajectories as JSON to file for viz")
      writePopulationTrajectoriesJSON(simulator.populationCompleted ++ simulator.population,  prefix + "_simulation_trajectories_" + simulator.ID + ".json", (simulator.startTime.value) to (simulator.finalTime.value) by (simulator.sf_dt.value))
      writeODJSON(simulator.populationCompleted ++ simulator.population, simulator.ODZones.map(_.name) ,prefix + "_ped_IDS_per_OD_" + simulator.ID + ".json")
    }
  }

  /**
    * Reads the files located in the argument and processes them to an  Iterable of [[ResultsContainerRead]] object.
    * @param path dir where the files are located
    * @return Iterable containing the results
    */
  def readResults(dir: Option[String]): Iterable[ResultsContainerRead] = {
    val path: String = dir match {
      case Some(str) => str
      case None => "tmp/"
    }
    val outputDir = new File(path)
    if (!outputDir.exists || !outputDir.isDirectory) {
      throw new IllegalArgumentException("Output dir for files does not exist ! dir=" + path)
    }

    // reads the files populates a map based on the keyword present in the name
    val files: Map[String, Map[String, File]] = outputDir.listFiles.filter(_.isFile).toList.groupBy(f => f.getName.substring(f.getName.indexOf(".csv")-10, f.getName.indexOf(".csv"))).map(kv => kv._1 -> kv._2.map(f => {
      f.getName match {
        case a if a.contains("_tt_") => "tt"
        case b if b.contains("_density_") => "density"
        case c if c.contains("_individual_densities_") => "individual_densities"
        case other => throw new IllegalArgumentException("File should not be present: " + other)
      }
    } -> f
    ).toMap)

    files.map(sr => {

      // process travel times file
      val tt: Vector[(String, String, Double, Double, Double)] = {
        val in = scala.io.Source.fromFile(sr._2("tt"))
        val data = (for (line <- in.getLines) yield {
          val cols = line.split(",").map(_.trim)
          (cols(0), cols(1), cols(2).toDouble, cols(3).toDouble, cols(4).toDouble)
        }).toVector
        in.close
        data
      }


      // process density file
        val density: Option[(Vector[Double], Vector[Vector[Double]])] =
          if (sr._2.keySet.contains("density")) {
          val in = scala.io.Source.fromFile(sr._2("density"))
          val data = (for (line <- in.getLines) yield {
            line.split(",").map(_.trim.toDouble)
          }).toVector
          in.close
          Some((data.map(_ (0)), data.map(a => a.tail.toVector))) //data.map(_.toVector).toVector
        } else { None }


      // process individual density measurements
        val densityPerIndividual: Option[Vector[(BigDecimal, BigDecimal)]] =
          if (sr._2.keySet.contains("individual_densities")) {
            val in = scala.io.Source.fromFile(sr._2("individual_densities"))
            val data = (for (line <- in.getLines if !line.contains("Infinity")) yield {
            Try(line.split(",").map(v => BigDecimal(v.trim))) match {
              case Success(s) => (s(0), s(1));
              case Failure(f) => throw new IllegalArgumentException("error parsing string to BigDecimal: " + line.split(",").map(v => v.trim).mkString(",") + ", " + sr._2("individual_densities") + ", " + f)
            }
          }).toVector
          in.close
          Some(data)
        } else { None }

      ResultsContainerRead(tt, density, densityPerIndividual)
    })
  }

  /** Creates, runs and makes a video from the simulation. No results are processed.
    * Making the video can take some time.
    */
  def runSimulationWithVideo(config: Config): Unit = {

    // create simulation
    val sim = createSimulation[PedestrianNOMAD](config)

    // Creates images representing the walls, route graph and both overlaid.
    val wallsImage = new DrawWalls(sim.walls, config.getString("output.output_prefix") + "_wallsWithNames.png", showNames = true)
    val graphImage = new DrawGraph(sim.graph.edges.map(e => (e.startVertex, e.endVertex)).toVector, config.getString("output.output_prefix") + "_graph.png")
    val fullImage = new DrawWallsAndGraph(sim.walls, sim.graph.edges.map(e => (e.startVertex, e.endVertex)).toVector, config.getString("output.output_prefix") + "_wallsAndGraph.png")
    val devicesImage = new DrawControlDevicesAndWalls(config.getString("output.output_prefix") + "_wallsWithDevices.png", sim.walls, sim.controlDevices)

    println("Running simulation for video...")

    // execute simulation
    timeBlock(sim.run())

    if (config.getBoolean("output.write_trajectories_as_JSON")) {
      println("Writing trajectory data as JSON...")
      writePopulationTrajectoriesJSON(sim.populationCompleted ++ sim.population,  config.getString("output.output_prefix") + "_simulation_trajectories_" + sim.ID + ".json", (sim.startTime.value) to (sim.finalTime.value) by (sim.sf_dt.value))
    }

    println("Making video of simulation, this can take some time...")

    val gates: List[BinaryGate] = List()

    new MovingPedestriansWithDensityWithWallVideo(
      config.getString("output.output_prefix") + "_moving_pedestrians_walls.mp4",
      sim.walls.filterNot(_.isInstanceOf[MovableWall]),
      math.max((1.0 / config.getDouble("output.video_dt")).toInt, 1),
      sim.populationCompleted ++ sim.population,
      sim.criticalAreas.values,
      gates.map(g => g.ID -> g).toMap,
      collection.mutable.ArrayBuffer(),
      scala.collection.mutable.ArrayBuffer(),
      (sim.startTime.value to sim.finalTime.value by config.getDouble("output.video_dt")).map(new Time(_)),
      sim.controlDevices.flowSeparators
    )
  }

  /** Runs the simulation and then collects the results. The simulation is timed.
    *
    * @param simulator simulation to run
    * @return results collected from the simulation
    */
  def runAndCollect[T <: PedestrianNOMAD](simulator: NOMADGraphSimulator[T]): ResultsContainerNew =  {
    timeBlock(simulator.run())
    collectResults(simulator)
  }


  def runAndWriteResults[T <: PedestrianNOMAD](simulator: NOMADGraphSimulator[T], prefix: String = "", dir: Option[String], writeTrajectoriesVS: Boolean = false, writeTrajectoriesJSON: Boolean = false): Unit = {
    timeBlock(simulator.run())
    writeResults(simulator, prefix, dir, writeTrajectoriesVS, writeTrajectoriesJSON)
  }


  // ******************************************************************************************
  //                    Read CLI arguments and process parameters file
  // ******************************************************************************************

  // Used to parse command line inputs
  case class CLInput(conf: String = "")

  def parseConfigFile(args: Array[String]): Config = {

    // Actually parses the command line arguments
    val parser = new scopt.OptionParser[CLInput]("scopt") {
      head("scopt", "3.x")

      opt[String]('c', "conf").required().valueName("<file>")
        .action((x, c) => c.copy(conf = x))
        .text("required, configuration file for the simulation")

      help("help").text("prints this usage text")
    }

    // Process the file passed as input and checks the format and parameters
    val confFileContents: String = parser.parse(args, CLInput()) match {
      case Some(conf) =>
        if (conf.conf.isEmpty) {
          throw new IllegalArgumentException("Configuration file is mandatory")
        }
        else {
          conf.conf
        }
      case None =>
        println("Error parsing CLI arguments, defaulting to reference.conf")
        "reference.conf"
    }

    // Reads the file passed as argument. Path is from execution path, i.e. where the program is run from (build.sbt usually)
    ConfigFactory.load(ConfigFactory.parseFile(new File(confFileContents)))
  }
}
