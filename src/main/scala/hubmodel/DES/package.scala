package hubmodel

import java.util.concurrent.ThreadLocalRandom

import com.typesafe.config.{Config, ConfigFactory}
import hubmodel.demand.flows.{ProcessDisaggregatePedestrianFlows, ProcessPedestrianFlows}
import hubmodel.demand.{PedestrianFlowFunction_New, PedestrianFlowPT_New, PedestrianFlow_New, ProcessTimeTable, PublicTransportSchedule, readDisaggDemand, readDisaggDemandTF, readPedestrianFlows, readSchedule, readScheduleTF}
import hubmodel.ped.PedestrianNOMAD
import hubmodel.supply.{NodeID_New, NodeParent, StopID_New, TrainID_New}
import hubmodel.supply.continuous.ReadContinuousSpace
import hubmodel.supply.graph.{Stop2Vertex, readGraph, readPTStop2GraphVertexMap}
import hubmodel.tools.Time
import hubmodel.tools.cells.Rectangle

import scala.collection.GenIterable
import scala.collection.parallel.ForkJoinTaskSupport
import scala.reflect.ClassTag

package object DES {

  /** Creates a simulation, but does not run it
    *
    * @return simulator ready to run
    */
  def createSimulation[T <: PedestrianNOMAD](config: Config, flows_TF: Option[String] = None, timetable_TF: Option[String] = None)(implicit tag: ClassTag[T]): NOMADGraphSimulator[T] = {




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

    // Builds the graph used for route choice. This Graph is coposed of multiple different link types.
    val (routeGraph, controlDevices) = readGraph[T](
      config.getString("files.graph"),
      config.getBoolean("sim.use_flow_gates"),
      config.getBoolean("sim.use_binary_gates"),
      config.getBoolean("sim.use_amw"),
      config.getBoolean("sim.use_flow_sep"),
      config.getBoolean("sim.fixed_flow_sep"),
      config.getBoolean("sim.measure_density"),
      config.getBoolean("sim.use_alternate_graphs")
    )

    val flows = getFlows(config)

    val (timeTable, stop2Vertex) =
      if (timetable_TF.isEmpty) {
        getPTSchedule(flows, config)
      } else {
        getPTSchedule(flows, timetable_TF.get, config)
      }

    val disaggPopulation =
      if (timetable_TF.isEmpty && flows_TF.isEmpty) {
        getDisaggPopulation(config)
      } else if (flows_TF.nonEmpty && timetable_TF.isEmpty) {
        getDisaggPopulation(config, flows_TF.get)
      } else {
        getDisaggPopulation(flows_TF.get)
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
      sf_dt = socialForceInterval,
      route_dt = routeUpdateInterval,
      evaluate_dt = evaluationInterval,
      rebuildTreeInterval = Some(rebuildTreeInterval),
      spaceMicro = infraSF.continuousSpace,
      graph = routeGraph,
      timeTable = timeTable,
      stop2Vertices = conceptualNode2GraphNodes,
      controlDevices = controlDevices,
      config.getBoolean("output.write_trajectories_as_VS") || config.getBoolean("output.write_trajectories_as_JSON") || config.getBoolean("output.make_video")
    )

    insertDemandIntoSimulator[T](sim, disaggPopulation, flows, timeTable)

    sim
  }


  def getFlows(config: Config): (Iterable[PedestrianFlow_New], Iterable[PedestrianFlowPT_New], Iterable[PedestrianFlowFunction_New]) = if (!config.getIsNull("files.flows") && config.getBoolean("sim.use_flows")) {
    readPedestrianFlows(config.getString("files.flows"))
  } else if (!config.getIsNull("files.flows_TF") && config.getBoolean("sim.use_flows")) {
    readPedestrianFlows(config.getString("files.flows_TF"))
  } else {
    println(" * using only disaggregate pedestrian demand")
    (Iterable(), Iterable(), Iterable())
  }

  def computeNumberOfSimulations(config: Config, multipleDemandSets: Option[Seq[(String, String)]]): Int = {
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

  def getIteratorForSimulations(numberThreads: Option[Int], numberSimulation: Int): GenIterable[Int] = {
    if (numberThreads.isDefined) {
      val r = (1 to numberSimulation).par
      r.tasksupport = new ForkJoinTaskSupport(new scala.concurrent.forkjoin.ForkJoinPool(numberThreads.get))
      r
    }
    else {
      1 to numberSimulation
    }
  }



  // Loads the train time table used to create demand from trains
  def getPTSchedule(flows: (Iterable[PedestrianFlow_New], Iterable[PedestrianFlowPT_New], Iterable[PedestrianFlowFunction_New]), config: Config): (PublicTransportSchedule, Stop2Vertex) = {
    if (!config.getIsNull("files.timetable")) {
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
      (new PublicTransportSchedule("unused", Vector()), new Stop2Vertex(Map(), Vector(Vector())))
    } else {
      throw new IllegalArgumentException("both time tables files are set to null in config file")
    }
  }

  // Loads the train time table used to create demand from trains
  def getPTSchedule(flows: (Iterable[PedestrianFlow_New], Iterable[PedestrianFlowPT_New], Iterable[PedestrianFlowFunction_New]),
                    timetable_TF: String,
                    config: Config,
                   ): (PublicTransportSchedule, Stop2Vertex) = {
    (
      readScheduleTF(timetable_TF),
      readPTStop2GraphVertexMap(config.getString("files.zones_to_vertices_map"))
    )
  }


  // Loads the disaggregate pedestrian demand.
  def getDisaggPopulation(config: Config): Iterable[(String, String, Option[Time])] = {
    if (config.getIsNull("files.flows_TF") && !config.getIsNull("files.disaggregate_demand")) {
      readDisaggDemand(config.getString("files.disaggregate_demand"))
        .flatMap(p =>
          if (!config.getIsNull("sim.increase_disaggregate_demand") && ThreadLocalRandom.current().nextDouble() >= (1.0-config.getDouble("sim.increase_disaggregate_demand")/100.0)) {
            Iterable(p, (p._1, p._2, Option(p._3.get.addDouble(ThreadLocalRandom.current().nextDouble(-15,15)))))
          } else {
            Iterable(p)
          }
        )
    } else if (config.getBoolean("sim.read_multiple_demand_sets")) {
      readDisaggDemand(config.getString("files.disaggregate_demand"))
        .flatMap(p =>
          if (!config.getIsNull("sim.increase_disaggregate_demand") && ThreadLocalRandom.current().nextDouble() >= (1.0-config.getDouble("sim.increase_disaggregate_demand")/100.0)) {
            Iterable(p, (p._1, p._2, Option(p._3.get.addDouble(ThreadLocalRandom.current().nextDouble(-15,15)))))
          } else {
            Iterable(p)
          }
        )
    } else if (config.getIsNull("files.disaggregate_demand") && !config.getIsNull("files.flows_TF")) {
      readDisaggDemandTF(config.getString("files.flows_TF"))
    } else {
      println(" * using only standard pedestrian flows")
      Iterable()
    }
  }

  def getDisaggPopulation(config: Config, file: String): Iterable[(String, String, Option[Time])] = {
      readDisaggDemand(config.getString("files.TF_demand_sets") + file)
        .flatMap(p =>
          if (!config.getIsNull("sim.increase_disaggregate_demand") && ThreadLocalRandom.current().nextDouble() >= (1.0-config.getDouble("sim.increase_disaggregate_demand")/100.0)) {
            Iterable(p, (p._1, p._2, Option(p._3.get.addDouble(ThreadLocalRandom.current().nextDouble(-15,15)))))
          } else {
            Iterable(p)
          }
        )
  }

  // Loads the disaggregate pedestrian demand.
  def getDisaggPopulation(flows_TF: String): Iterable[(String, String, Option[Time])] = {
    readDisaggDemandTF(flows_TF)
  }


  def insertDemandIntoSimulator[T <: PedestrianNOMAD](sim: NOMADGraphSimulator[T],
                                                      disaggPopulation: Iterable[(String, String, Option[Time])],
                                                      flows: (Iterable[PedestrianFlow_New], Iterable[PedestrianFlowPT_New], Iterable[PedestrianFlowFunction_New]),
                                                      timeTable: PublicTransportSchedule)(implicit tag: ClassTag[T]): Unit = {

    if (disaggPopulation.nonEmpty) {
      val newDisaggPopulation = disaggPopulation/*.groupBy(p => (p._1, p._2)).flatMap(gp => gp._2.take(2))*/
      sim.insertEventWithZeroDelay(new ProcessDisaggregatePedestrianFlows[T](newDisaggPopulation, sim))
    }

    val PTInducedFlows = flows._2.toVector
    sim.insertEventWithZeroDelay(new ProcessTimeTable[T](timeTable, PTInducedFlows, sim))
    sim.insertEventWithZeroDelay(new ProcessPedestrianFlows[T](flows._1, flows._3, sim))

  }
}
