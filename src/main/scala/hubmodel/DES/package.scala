package hubmodel

import java.util.concurrent.ThreadLocalRandom

import com.typesafe.config.{Config, ConfigFactory}
import hubmodel.demand.flows.{ProcessDisaggregatePedestrianFlows, ProcessDisaggregatePedestrianFlowsWithoutTimeTable, ProcessPedestrianFlows}
import hubmodel.demand.transit.Vehicle
import hubmodel.demand.{AggregateFlows, DemandData, DemandSet, PedestrianFlowFunction_New, PedestrianFlowPT_New, PedestrianFlow_New, ProcessTimeTable, PublicTransportSchedule, TRANSFORMDemandSet, readDisaggDemand, readDisaggDemandTF, readPedestrianFlows, readSchedule, readScheduleTF}
import hubmodel.ped.PedestrianNOMAD
import hubmodel.supply.continuous.ReadContinuousSpace
import hubmodel.supply.graph.{GraphContainer, Stop2Vertex, readGraph, readPTStop2GraphVertexMap}
import hubmodel.supply.{NodeID_New, NodeParent, StopID_New, TrainID_New}
import tools.Time
import tools.cells.Vertex

import scala.collection.parallel.CollectionConverters._
import scala.collection.parallel.ForkJoinTaskSupport
import scala.collection.parallel.immutable.ParVector
import scala.reflect.ClassTag

package object DES {


  /** Creates a simulation, the corresponding output directory if it doesn't exist and the runs and writes the
    * simulation to the output files.
    *
    * @param demand Tuples containing the demand
    * @param config Config with the simulation parameters
    */
  def createRunWriteSimulation(demand: Option[DemandData], config: Config): Unit = {

    // Creates the simulation
    val sim = demand match {
      case Some(_) => {
        createSimulation(config, demand)
      }
      case None => {
        createSimulation(config)
      }
    }

    // Creates the output directory
    val outputDir: String = if (config.getBoolean("files.multiple_demand_sets") || config.getBoolean("files.multiple_demand_sets_TF")) {
      config.getString("output.dir") + demand.get.dir.getFileName + "/" + demand.get.flowFile.getFileName.toString.replace(".json", "") + "/"
    } else {
      config.getString("output.dir")
    }

    // Runs the simulation and writes the results to the files
    runAndWriteResults(
      sim,
      config.getString("output.output_prefix") + "_",
      outputDir,
      config.getBoolean("output.write_trajectories_as_VS"),
      config.getBoolean("output.write_trajectories_as_JSON"),
      config.getBoolean("output.write_tt_4_transform")
    )

    // Calls the GC to clear memory
    System.gc()
  }


  /** Takes a conceptual node (train or on foot) and returns the set of "real" nodes (the ones used by the graph)
    * in which the pedestrians must be created.
    *
    * @param stop2Vertices  mapping from stops to vertices
    * @param timeTable      collection of PT movements
    * @param conceptualNode Node to map to graph nodes
    * @return
    */
  def mappingConceptualNode2GraphNodes(routeGraph: GraphContainer)(stop2Vertices: Map[StopID_New, Vector[VertexID]], timeTable: Map[TrainID_New, Vehicle])(conceptualNode: NodeParent): Iterable[Vertex] = {
    conceptualNode match {
      case x: TrainID_New => stop2Vertices(timeTable(x).stop).map(n => routeGraph.vertexMapNew(n))
      case x: NodeID_New => Iterable(routeGraph.vertexMapNew(x.ID))
      case x: StopID_New => Iterable(routeGraph.vertexMapNew(x.ID.toString))
      case _ => throw new Exception("Track ID should not be there !")
    }
  }

  /** Creates a simulation, but does not run it
    *
    * @return simulator ready to run
    */
  def createSimulation(config: Config, demandSet: Option[DemandData] = None)(implicit tag: ClassTag[PedestrianNOMAD]): NOMADGraphSimulator = {


    /*if ((timetable_TF.isEmpty && flows_TF.isDefined) || (timetable_TF.isDefined && flows_TF.isEmpty)) {
      throw new IllegalArgumentException("Input to create function is wrong ! Either both optional arguments must be set or none of them")
    }*/

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

    // Builds the graph used for route choice. This Graph is composed of multiple different link types.
    val (routeGraph, controlDevices) = readGraph(
      config.getString("files.graph"),
      config.getBoolean("sim.use_flow_gates"),
      config.getBoolean("sim.use_binary_gates"),
      config.getBoolean("sim.use_amw"),
      config.getBoolean("sim.use_flow_sep"),
      config.getBoolean("sim.fixed_flow_sep"),
      config.getBoolean("sim.measure_density"),
      config.getBoolean("sim.use_alternate_graphs")
    )

    // Reads the pedestrian flows which are modelled as flows (i.e. aggregate)
    val flows = getAggregateFlows(config)

    // Reads the PT timetable. The aggregate flows are needed to filter the PT vehicles.
    val (timeTable, stop2Vertex) = getPTSchedule(config, demandSet, flows._2.nonEmpty)


    val disaggPopulation = getDisaggregateFlows(config, demandSet)
    /*if (demandSet.isEmpty) { // no

      }/* else if (flows_TF_file.nonEmpty && timetable_TF_file.isEmpty) {
        getDisaggregateFlows(config, flows_TF_file.get)
      }*/ else {
        getDisaggPopulation(demandSet.get._1.name)
      }*/

    // Loads the start time, end time and time intervals
    val simulationStartTime: Time = Time(config.getDouble("sim.start_time"))
    val simulationEndTime: Time = Time(config.getDouble("sim.end_time"))
    val socialForceInterval: Time = Time(config.getDouble("sim.sf_dt"))
    val routeUpdateInterval: Time = Time(config.getDouble("sim.route_dt"))
    val evaluationInterval: Time = Time(config.getDouble("sim.evaluate_dt"))
    val rebuildTreeInterval: Time = Time(config.getDouble("sim.rebuild_tree_dt"))

    // Loads the prediction parameters
    val predictionParams: PredictionInputParameters = new PredictionInputParameters(
      Time(config.getDouble("sim.prediction.horizon")),
      Time(config.getDouble("sim.prediction.update-interval")),
      Time(config.getDouble("sim.prediction.dv-length")),
      Time(config.getDouble("sim.prediction.density-msmt-update")),
      config.getInt("sim.prediction.replications"),
      config.getInt("sim.prediction.threads")
    )

    val simulationParameters: SimulationInputParameters = new SimulationInputParameters(
      simulationStartTime,
      simulationEndTime,
      socialForceInterval,
      routeUpdateInterval,
      infraSF.continuousSpace.addWalls(controlDevices.amws.flatMap(_.walls)),
      routeGraph,
      stop2Vertex,
      controlDevices,
      predictionParams
    )

    simulationParameters.rebuildTreeInterval = Some(rebuildTreeInterval)
    simulationParameters.logFullPedestrianHistory = config.getBoolean("output.write_trajectories_as_VS") || config.getBoolean("output.write_trajectories_as_JSON") || config.getBoolean("output.make_video")
    simulationParameters.timeTable = timeTable
    simulationParameters.stateEvaluationInterval = Some(evaluationInterval)

    // Creation of the simulator
    val sim: NOMADGraphSimulator = new PedestrianSimulation(simulationParameters)

    // Insertion of the demand (pedestrian flows and PT vechicles) into the simulator
    insertDemandIntoSimulator(sim, disaggPopulation, flows, timeTable)

    // returns the simulator
    sim
  }


  /** Reads the aggregate pedestrian flows. The [[AggregateFlows]] is a tuple containing
    *  - the uniform flows
    *  - the PT induced flows
    *  - the functional flows
    *
    * @param config config file
    * @return [[AggregateFlows]] tuple with the three kind of flows
    */
  def getAggregateFlows(config: Config): AggregateFlows = {

    if (config.getBoolean("sim.use_aggregate_demand")) {
      readPedestrianFlows(config.getString("files.aggregate_demand"))
    } else {
      println(" * not using aggregate pedestrian flows")
      (Iterable(), Iterable(), Iterable())
    }
  }

  /** Determines the number of simulations to run. The number of simulations is either
    *  - the number of demand sets (one replication each),
    *  - the number of simulations specified in the config file.
    *
    * @param config             config file used to read the number of simulations
    * @param multipleDemandSets demand sets used for simulations
    * @return total number of simulations to run
    */
  def computeNumberOfSimulations(config: Config, multipleDemandSets: Option[Seq[DemandData]]): Int = {
    if (multipleDemandSets.isDefined) {
      println(" * using " + multipleDemandSets.get.length + " different pedestrian demand sets")
      println(" * performing " + config.getInt("sim.nb_runs") + " replication(s) per demand set")
      multipleDemandSets.get.length
    } else {
      println(" * using demand from flow files")
      println(" * running " + config.getInt("sim.nb_runs") + " simulations")
      config.getInt("sim.nb_runs")
    }
  }

  def getParallelVectorForSimulations(numberThreads: Int, numberSimulation: Int): ParVector[Int] = {
    val range: ParVector[Int] = Vector.range(0, numberSimulation).par
    range.tasksupport = new ForkJoinTaskSupport(new java.util.concurrent.ForkJoinPool(math.min(numberSimulation, numberThreads)))
    range
  }


  /** Reads the public transport schedule based from either the demand set file or the timetable field in the config
    * file. The mapping from stops to vertices is empty if the schedule is not defined.
    *
    * @param config    config file
    * @param demandSet demand set to read PT schedule from
    * @return tuple containing an optional [[PublicTransportSchedule]] and the [[Stop2Vertex]] object
    */
  def getPTSchedule(config: Config, demandSet: Option[DemandData] = None, usePTInducedFlow: Boolean = false): (Option[PublicTransportSchedule], Stop2Vertex) = {

    // Reads the schedule.
    val schedule: Option[PublicTransportSchedule] = {
      demandSet match {
        case Some(ds: DemandSet) => {
          Some(readSchedule(ds.timetableFile.toString))
        }
        case Some(tf: TRANSFORMDemandSet) => {
          Some(readScheduleTF(tf.timetableFile.toString))
        }
        case None if usePTInducedFlow => {
          Some(readSchedule(config.getString("files.timetable")))
        }
        case _ => {
          println(" * no time table is required as PT induced flows are empty");
          None
        }
      }
    }

    // Mapping from stops to vertices in the network.
    val stop2vertexMap = {
      if (schedule.isDefined) {
        readPTStop2GraphVertexMap(config.getString("files.zones_to_vertices_map"))
      }
      else {
        new Stop2Vertex(Map(), Vector())
      }
    }

    (schedule, stop2vertexMap)
  }


  // Loads the disaggregate pedestrian demand.
  def getDisaggregateFlows(config: Config, demandSet: Option[DemandData] = None): Vector[(String, String, Option[Time])] = {

    val disaggDemand: Vector[(String, String, Option[Time])] = demandSet match {
      case Some(ds: DemandSet) => {
        readDisaggDemand(ds.flowFile.toString)
      }
      case Some(tf: TRANSFORMDemandSet) => {
        readDisaggDemandTF(tf.flowFile.toString)
      }
      case None if !config.getIsNull("files.disaggregate_demand") => {
        readDisaggDemand(config.getString("files.disaggregate_demand"))
      }
      case _ => {
        println(" * not using disaggregate pedestrian flows");
        Vector()
      }
    }

    if (config.getIsNull("sim.increase_disaggregate_demand")) {
      disaggDemand
    } else {
      disaggDemand.flatMap(p => increaseDemand(config.getDouble("sim.increase_disaggregate_demand"), p))
    }
  }

  /*@deprecated
  def getDisaggPopulation(config: Config, file: String): Iterable[(String, String, Option[Time])] = {
    readDisaggDemand(config.getString("files.demand_sets") + file)
      /*.flatMap(p =>
        if (!config.getIsNull("sim.increase_disaggregate_demand") && ThreadLocalRandom.current().nextDouble() >= (1.0 - config.getDouble("sim.increase_disaggregate_demand") / 100.0)) {
          Iterable(p, (p._1, p._2, Option(p._3.get.addDouble(ThreadLocalRandom.current().nextDouble(-15, 15)))))
        } else {
          Iterable(p)
        }
      )*/
  }
*/
  /** Add a pedestrian with a given probability. This method can only increase the demand, it cannot decrease it.
    * The input pedestrian will be copied with a given probability. The OD data is identical but the entry time
    * into the simulation is modified. The entry time of the new pedestrian is between 15  seconds earlier or later
    * than the original entry time.
    *
    * @param ratio      percentage increase in demand.
    * @param pedestrian pedestrian to copy
    * @return one or two pedestrians depending on whether a pedestrian has been added or not.
    */
  def increaseDemand(ratio: Double, pedestrian: (String, String, Option[Time])): Iterable[(String, String, Option[Time])] = {
    if (ThreadLocalRandom.current().nextDouble() > (1.0 - ratio / 100.0)) {
      Iterable(pedestrian, (pedestrian._1, pedestrian._2, Option(pedestrian._3.get.addDouble(ThreadLocalRandom.current().nextDouble(-15, 15)))))
    } else {
      Iterable(pedestrian)
    }
  }

  // Loads the disaggregate pedestrian demand.
  def getDisaggPopulation(flows_TF: String): Iterable[(String, String, Option[Time])] = {
    readDisaggDemandTF(flows_TF)
  }


  /** Inserts the demand into the simulator. Four types of pedestrian flows are inserts:
    *  - disaggregate pedestrian flows
    *  - uniform pedestrian flows
    *  - flows originating from PT vehicles
    *  - non unniform flows
    *
    * @param sim              simulator into which the demand must be inserted
    * @param disaggPopulation disaggregate pedestrian flows
    * @param flows            aggregate pedestrian flows
    * @param timeTable        PT schedule
    * @param tag              Pedestrian type
    */
  def insertDemandIntoSimulator(sim: NOMADGraphSimulator,
                                                      disaggPopulation: Iterable[(String, String, Option[Time])],
                                                      flows: (Iterable[PedestrianFlow_New], Iterable[PedestrianFlowPT_New], Iterable[PedestrianFlowFunction_New]),
                                                      timeTable: Option[PublicTransportSchedule])(implicit tag: ClassTag[PedestrianNOMAD]): Unit = {

    // inserts disaggregate pedestrian flows.
    if (disaggPopulation.nonEmpty) {
      timeTable match {
        case Some(_) => {
          sim.insertEventWithZeroDelay(new ProcessDisaggregatePedestrianFlows(disaggPopulation, sim))
        }
        case None => {
          sim.insertEventWithZeroDelay(new ProcessDisaggregatePedestrianFlowsWithoutTimeTable(disaggPopulation, sim))
        }
      }
    }


    // Inserts PT vehicles and associated pedestrian flows.
    if (timeTable.isDefined && flows._2.nonEmpty) {
      sim.insertEventWithZeroDelay(new ProcessTimeTable(timeTable.get, flows._2.toVector, sim))
    }

    // Inserts aggregate pedestrian flows.
    if (flows._1.nonEmpty || flows._3.nonEmpty) {
      sim.insertEventWithZeroDelay(new ProcessPedestrianFlows(flows._1, flows._3, sim))
    }
  }
}
