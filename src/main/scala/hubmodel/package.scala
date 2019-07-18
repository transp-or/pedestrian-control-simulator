import java.io.{BufferedWriter, File, FileWriter}
import java.nio.file.{Files, Paths}
import java.util.concurrent.ThreadLocalRandom

import com.typesafe.config.{Config, ConfigFactory, ConfigValueFactory}
import hubmodel.DES.{NOMADGraphSimulator, _}
import hubmodel.demand.PublicTransportSchedule
import hubmodel.io.output.image.{DrawControlDevicesAndWalls, DrawGraph, DrawWalls, DrawWallsAndGraph}
import hubmodel.io.output.video.MovingPedestriansWithDensityWithWallVideo
import hubmodel.mgmt.ControlDevices
import hubmodel.mgmt.flowgate.BinaryGate
import hubmodel.ped.History.HistoryContainer
import hubmodel.ped.{PedestrianNOMAD, PedestrianSim, PedestrianTrait}
import hubmodel.results.{ResultsContainerFromSimulation, collectResults, writeResults}
import hubmodel.supply.NodeParent
import hubmodel.supply.continuous.{ContinuousSpace, MovableWall}
import hubmodel.supply.graph._
import hubmodel.tools.Time
import hubmodel.tools.cells.Rectangle
import myscala.math.vector.{Vector2D, Vector3D}
import myscala.timeBlock
import org.apache.commons.lang3.RandomStringUtils

import scala.collection.immutable.NumericRange


/**
  * Created by nicholas on 5/12/17.
  */

package object hubmodel {


  ////////////////////////// SIMULATION PARAMETERS /////////////////////////////////

  // each pedestrians contains the list of walls which he is close to. This means there is no need to compute at every
  // time step the interaction with all the walls.
  val DISTANCE_TO_CLOSE_WALLS: Double = 10.0

  // Length in meters of the othogonal extension of the "near region" for flow lines. This should be larger than the
  // maximum walking speed * the motion model clock (sf_dt) to ensure a pedestrian cannot "jump" over it. The larger the
  // value of the parameter, the more pedestrians will be checked to see if they cross the line..
  val FLOW_LINE_REGION_EXTENSION: Double = 2.0

  // Change in opposing flow fractions which is considered significant. The flow separators will only move if the change
  // is larger than this value
  val FLOW_SEPARATOR_UPDATE: Double = 0.1 // 10%

  // Speed of the moving separator.
  val FLOW_SEPARATOR_SPEED: Double = 0.25 // m/s fixed in a arbitrary manner

  // Maximum number of pedestrians who can wait behind a gate. This prevents simulations  becoming infeasible when
  // hundreds of pedestrians are stuck behind a  gate and then the simulation time explodes.
  val GATE_MAXIMUM_QUEUE_SIZE: Int = 15

  /** Samples a pedestrian free flow walking speed.
    *
    * Either use the standard mean of 1.34 m/s. Otherwise to match the tracking data use 1.10 as a mean.
    *
    * @return walking speed
    */
  def pedestrianWalkingSpeed: Double = 1.34 + math.min(0.2 * ThreadLocalRandom.current().nextGaussian(), 3.0)


  type GroupID = Int

  /* pedestrian isolations */
  val ISOLATED: Int = 0
  val IN_RANGE: Int = 1
  val IN_COLLISION: Int = 2

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

  /** Generation of a pseudo-UUID. This can be used to generate unique identifiers for objects.
    *
    * @return UUID formatted as a String
    */
  def generateUUID: String = RandomStringUtils.randomAlphabetic(1) + RandomStringUtils.randomAlphanumeric(10) // java.util.UUID.randomUUID.toString


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
    val tmpConfig: Config = ConfigFactory.load(ConfigFactory.parseFile(new File(confFileContents)))

    // checks that the output directory exists, if not, defaults to tmp/
    if (!Files.exists(Paths.get(tmpConfig.getString("output.dir")))) {
      val id: String = generateUUID
      Files.createDirectory(Paths.get("tmp/" + id + "/"))
      println("WARNNING ! Output directory does not exist ! Defaulting to tmp/" + id + "/")
      tmpConfig.withValue("output.dir", ConfigValueFactory.fromAnyRef("tmp/" + id + "/"))
    } else {
      tmpConfig
    }
  }

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


  /** Write the trajectories as a JSON format.
    *
    * @param pop
    * @param file
    * @param times
    */
  def writePopulationTrajectoriesJSON(pop: Iterable[PedestrianSim], file: String, times: NumericRange[BigDecimal]): Unit = {

    // collects the traj data
    val trajDataByTime: Map[Time, Iterable[(Time, HistoryContainer, String)]] = pop.flatMap(p => p.getHistoryPosition.map(r => (r._1, r._2, p.ID))).groupBy(_._1).filter(t => times.contains(t._1.value))

    val sortedKeys: Vector[Time] = trajDataByTime.keys.toVector.sorted

    // opens the file for writing
    val f = new File(file)
    val bw = new BufferedWriter(new FileWriter(f))
    bw.write("[")
    // write first value separately to deal with commas seperating
    bw.write("{\"time\":" + sortedKeys.head.toString + ",")
    bw.write("\"data\":[" + trajDataByTime(sortedKeys.head).map(d => "{\"id\":\"" + d._3 + "," + d._2.toJSON + "}").mkString(","))
    bw.write("]}")
    for (t <- sortedKeys.tail) {
      bw.write(",{\"time\":" + t.toString + ",")
      bw.write("\"data\":[")
      bw.write(trajDataByTime(t).map(d => "{\"id\":\"" + d._3 + "," + d._2.toJSON + "}").mkString(","))
      bw.write("]}")
    }

    bw.write("]")
    bw.close()
  }

  def writeODJSON(pop: Iterable[PedestrianSim], ODZones: Iterable[String], file: String): Unit = {
    def buildStringFromIDs(IDs: Iterable[String]): String = {
      if (IDs.isEmpty) {
        ""
      }
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

  // Loads the pedestrian flows. These are either exogenous to the trains (from outside) or flows originating from trains.


  // ******************************************************************************************
  //                    Runs the simulation, creates video and outputs results
  // ******************************************************************************************


  /** Creates, runs and makes a video from the simulation. No results are processed.
    * Making the video can take some time.
    */
  def runSimulationWithVideo(config: Config, singleDemandSet: Option[String] = None): Unit = {

    // create simulation
    val sim = createSimulation[PedestrianNOMAD](config, singleDemandSet)

    // Creates images representing the walls, route graph and both overlaid.
    new DrawWalls(sim.walls, config.getString("output.output_prefix") + "_wallsWithNames.png", showNames = true)
    sim.graph match {
      case rm: MultipleGraph => {
        rm.getGraphs.foreach(g => new DrawGraph(g._2._2.edgeCollection.map(e => (e.startVertex, e.endVertex)).toVector, config.getString("output.output_prefix") + "_graph_" + g._1 + ".png"))
      }
      case rs: SingleGraph => {
        new DrawGraph(sim.graph.edges.map(e => (e.startVertex, e.endVertex)).toVector, config.getString("output.output_prefix") + "_graph.png")
      }
    }
    new DrawWallsAndGraph(sim.walls, sim.graph.edges.map(e => (e.startVertex, e.endVertex)).toVector, config.getString("output.output_prefix") + "_wallsAndGraph.png")
    new DrawControlDevicesAndWalls(config.getString("output.output_prefix") + "_wallsWithDevices.png", sim.walls, sim.controlDevices)

    println("Running simulation for video...")

    // execute simulation
    timeBlock(sim.run())

    if (config.getBoolean("output.write_trajectories_as_JSON")) {
      println("Writing trajectory data as JSON...")
      writePopulationTrajectoriesJSON(sim.populationCompleted ++ sim.population, config.getString("output.output_prefix") + "_simulation_trajectories_" + sim.ID + ".json", (sim.startTime.value) to (sim.finalTime.value) by (sim.sf_dt.value))
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
    * This method works for short and few simulations. For longer and many simulations, the intermediate
    * results must be written to  files otherwise the RAM will be full and everything will crash.
    *
    * @param simulator simulation to run
    * @return results collected from the simulation
    */
  def runAndCollect[T <: PedestrianNOMAD](simulator: NOMADGraphSimulator[T]): ResultsContainerFromSimulation = {
    timeBlock(simulator.run())
    collectResults(simulator)
  }

  /**
    * Takes a simulation as input and will run it then write the results to a file.
    *
    * @param simulator             simulation to run
    * @param prefix                prefix for all output files
    * @param dir                   directory where to write the intermediate results
    * @param writeTrajectoriesVS   should the trajectories be written as VisioSafe format ?
    * @param writeTrajectoriesJSON should the trajectories be written as CSV format ?
    * @param writeTRANSFORMTT      should the outputs for TRANS-FORM be computed and written ?
    * @tparam T pedestrian type used for the simulation
    */
  def runAndWriteResults[T <: PedestrianNOMAD](simulator: NOMADGraphSimulator[T], prefix: String = "", dir: String, writeTrajectoriesVS: Boolean = false, writeTrajectoriesJSON: Boolean = false, writeTRANSFORMTT: Boolean = false): Unit = {
    timeBlock(simulator.run())
    writeResults(simulator, prefix, dir, writeTrajectoriesVS, writeTrajectoriesJSON, writeTRANSFORMTT)
  }


  @deprecated
  type SimulatorParameters = (
    Time,
      Time,
      Time,
      Time,
      Time,
      Option[Time],
      ContinuousSpace,
      GraphContainer,
      PublicTransportSchedule,
      NodeParent => Iterable[Rectangle],
      ControlDevices
    )


}


