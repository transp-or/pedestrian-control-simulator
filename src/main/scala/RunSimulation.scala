
import java.io.File
import java.nio.file.{DirectoryStream, Files, Path, Paths}

import com.typesafe.config.{Config, ConfigFactory}
import hubmodel.DES.NOMADGraphSimulator
import hubmodel._
import hubmodel.output.TRANSFORM.PopulationSummaryProcessingTRANSFORM
import hubmodel.ped.{PedestrianNOMAD, PedestrianNOMADWithGraph}
import hubmodel.results.PopulationSummaryProcessing
import hubmodel.supply.StopID_New
import hubmodel.supply.graph.readPTStop2GraphVertexMap
import hubmodel.tools.IllegalSimulationInput
import myscala.math.stats.{ComputeQuantiles, ComputeStats, computeQuantiles}
import myscala.output.SeqOfSeqExtensions.SeqOfSeqWriter
import myscala.output.SeqTuplesExtensions.SeqTuplesWriter
import trackingdataanalysis.visualization.{Histogram, PlotOptions, ScatterPlot, computeHistogramDataWithXValues}

import scala.collection.GenIterable
import scala.collection.JavaConverters._
import scala.collection.parallel.ForkJoinTaskSupport
import scala.io.Source
import scala.collection.JavaConversions._
import scala.util.Try


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
  val config: Config = parseConfigFile(args)


  // ******************************************************************************************
  //                           Processes and writes results to CSV
  // ******************************************************************************************


  // Checks that the number of simulations to run is coherent
  /*if ( config.getInt("sim.nb_runs") == 0 && !config.getBoolean("output.make_video")) {
    throw new IllegalArgumentException("No simulation to run ! Check parameters in config file.")
  }*/


  val demandSets: Option[Seq[(String, String)]] = if (config.getBoolean("sim.read_multiple_TF_demand_sets")) {

    if (!((Paths.get(config.getString("files.TF_demand_sets")).toString == Paths.get(config.getString("files.flows_TF")).getParent.toString) &&
      (Paths.get(config.getString("files.flows_TF")).getParent.toString == Paths.get(config.getString("files.timetable_TF")).getParent.toString))) {
      throw new IllegalSimulationInput("Directories for multiple demand sets do not match !")
    }

    val multipleDemandStream: DirectoryStream[Path] = Files.newDirectoryStream(Paths.get(config.getString("files.TF_demand_sets")), "*.json")

    val files: Vector[Path] = multipleDemandStream.toVector

    multipleDemandStream.close()

    val flowBaseName: String = Paths.get(config.getString("files.flows_TF")).getFileName.toString.replace(".json", "")
    val timetableBaseName: String = Paths.get(config.getString("files.timetable_TF")).getFileName.toString.replace(".json", "")

    try {
      if (files.size % 2 != 0) {
        throw new IllegalSimulationInput("Uneven number of files for multiple demand sets ! (" + files.size + " files found)")
      } else if (files.isEmpty) {
        throw new IllegalSimulationInput("No files for multiple demand sets !")
      } else if (files.size == 2) {
        println("Warning ! Only one set of demands used for the multiple demand inputs. ")
        Some(
          Seq((
            files.find(_.getFileName.toString.contains(flowBaseName)).get.toString,
            files.find(_.getFileName.toString.contains(timetableBaseName)).get.toString
          )
          )
        )
      } else {
        Some(
          files
            .groupBy(f => f.getFileName.getFileName.toString.split("_").last.replace(".json", ""))
            .map(grouped => (grouped._2.find(_.getFileName.toString.contains(flowBaseName)).get.toString, grouped._2.find(_.getFileName.toString.contains(timetableBaseName)).get.toString)).toVector
        )
      }
    } catch {
      case e: Exception => throw e
    }
  } else {
    None
  }

  val simulationStartTime: Time = Time(config.getDouble("sim.start_time"))
  val simulationEndTime: Time = Time(config.getDouble("sim.end_time"))
  val n: Int = if (config.getBoolean("sim.read_multiple_TF_demand_sets")) {
    println(" * using " + demandSets.get.size + " different pedestrian demand sets")
    println(" * ignoring number of simulation runs")
    demandSets.get.size
  } else {
    println(" * running " + config.getInt("sim.nb_runs") + " simulations")
    config.getInt("sim.nb_runs")
  }
  val runSimulationsInParallel: Boolean = config.getBoolean("execution.parallel")
  val evaluationInterval: Time = Time(config.getDouble("sim.evaluate_dt"))


  // Runs the simulations in parallel or sequential based on the config file.

  if (config.getBoolean("output.make_video")) {
    println("Running simulation for video...")
    runSimulationWithVideo(config)
  }

  val range: GenIterable[Int] = if (runSimulationsInParallel) {
    val r = (1 to n).par
    r.tasksupport = new ForkJoinTaskSupport(new scala.concurrent.forkjoin.ForkJoinPool(config.getInt("execution.threads")))
    r
  }
  else {
    1 to n
  }

  if (n > 0) {
    range.foreach(s => {
      val sim =
        if (demandSets.isDefined) {
          createSimulation[PedestrianNOMAD](config, Some(demandSets.get(s-1)._1), Some(demandSets.get(s-1)._2))
        }
        else {
          createSimulation[PedestrianNOMAD](config)
        }
      runAndWriteResults(sim, config.getString("output.output_prefix") + "_", if (!config.getIsNull("output.dir")) Some(config.getString("output.dir")) else {
        None
      }, config.getBoolean("output.write_trajectories_as_VS"), config.getBoolean("output.write_trajectories_as_JSON"))
      System.gc()
    })
  } else {
    println("No more simulations to run !")
  }


  // Reads intermediate results
  val results: Vector[ResultsContainerRead] = readResults(if (!config.getIsNull("output.dir")) {
    Some(config.getString("output.dir"))
  } else {
    None
  }).toVector


  // Processing results

  println("Processing results")

  // Collects then writes individual travel times to csv
  if (config.getBoolean("output.write_travel_times")) results
    .map(r => r.tt.map(_._3))
    .writeToCSV(
      config.getString("output.output_prefix") + "_travel_times.csv",
      columnNames = Some(Vector.fill(results.size)("r").zipWithIndex.map(t => t._1 + t._2.toString)),
      rowNames = None
    )

  // Collects then writes individual travel times with OD to csv
  /*if (config.getBoolean("output.write_travel_times")) results
    .zipWithIndex
    .flatMap(r => r._1.tt.map(p => (r._2, p._3, p._1, p._2)))
    .writeToCSV(
      config.getString("output.output_prefix") + "_travel_times_OD.csv",
      columnNames = Some(Vector("run", "travel_time", "origin_id", "destination_id")),
      rowNames = None
    )*/

  if (config.getBoolean("output.write_densities")) {
    // Collects times at which densities where measured
    val densityTimes: Vector[Time] = results.head.monitoredAreaDensity.get._1.map(Time(_))

    // writes densities to csv, first column is time, second column is mean, third column is var, then all individual densities
    if (config.getBoolean("output.write_densities") && config.getBoolean("sim.measure_density")) {
      for (i <- results.head.monitoredAreaDensity.get._2.indices) {

        val densityStatsPerTime: Vector[(Int, Double, Double, Double, Double, Double)] = for (j <- densityTimes) yield {
          (for (k <- results.map(_.monitoredAreaDensity.get)) yield {
            k._2(i)(k._1.indexOf(j.value))
          }).stats
        }

        val densities: Vector[Vector[Double]] = results.map(_.monitoredAreaDensity.get._2(i))
        (for (ii <- densityStatsPerTime.indices) yield {
          Vector(densityStatsPerTime(ii)._1, densityStatsPerTime(ii)._2, densityStatsPerTime(ii)._3, densityStatsPerTime(ii)._4,
            densityStatsPerTime(ii)._5, densityStatsPerTime(ii)._6) ++ densities.map(_ (ii))
        }).transpose.writeToCSV(
          config.getString("output.output_prefix") + "_" + i + "_densities.csv",
          rowNames = Some(densityTimes.map(_.toString)),
          columnNames = Some(Vector("time", "size", "mean", "variance", "median", "min", "max") ++ Vector.fill(results.size)("r").zipWithIndex.map(t => t._1 + t._2.toString))
        )
      }
    }
  }

  if (config.getBoolean("output.write_density_stats")) {

    /* analyse traj data per density threshold */
    val targetDensityRange = BigDecimal(1.0) to BigDecimal(4.0) by BigDecimal(0.25)

    val individualDensityAboveThreshold: Map[BigDecimal, Vector[(BigDecimal, Int)]] = targetDensityRange.map(rho => rho -> {
      results.flatMap(r => r.monitoredAreaIndividualDensity.get).groupBy(d => d._1).map(v => v._1 -> v._2.count(_._2 > rho) / results.size)
    }).toMap.map(v => v._1 -> v._2.toVector.sortBy(_._1))

    individualDensityAboveThreshold.toVector.sortBy(_._1).map(_._2.map(_._2)).writeToCSV(config.getString("output.output_prefix") + "-pax-above-target.csv", rowNames = None, columnNames = Some(targetDensityRange.map(_.toString)))

    new ScatterPlot(config.getString("output.output_prefix") + "_time-pax-above-threshold.png",
      individualDensityAboveThreshold(2.0).map(_._1.toDouble),
      individualDensityAboveThreshold(2.0).map(_._2.toDouble),
      "time [s]",
      "# individuals with density above threshold",
      PlotOptions(ymax = Some(80))
    )

    val binSize = 0.15
    val opts = PlotOptions(xmin = Some(0), xmax = Some(10), ymax = Some(0.05), width = 700, height = 400)
    val dataAgg = results.flatMap(_.monitoredAreaDensity.get._2.flatten).filter(_ > 0)
    new Histogram(config.getString("output.output_prefix") + "_density-histogram.png",
      dataAgg,
      binSize,
      "densities [pax/m^2]",
      "Histogram of densities measured in area",
      opts
    )
    //println("density " + results.flatMap(_.monitoredAreaDensity.get._2.flatten).stats)
    //println(computeQuantiles(Vector(65,70,75,80,85,90,95,97,99))(results.flatMap(_.monitoredAreaDensity.get._2.flatten)))
    computeHistogramDataWithXValues(dataAgg, binSize, opts.xmin, opts.xmax).writeToCSV(config.getString("output.output_prefix") + "_density-hist_data.csv", rowNames = None, columnNames = Some(Vector("x", "y")))

    val dataDisagg = results.flatMap(_.monitoredAreaIndividualDensity.get.map(_._2))
    new Histogram(config.getString("output.output_prefix") + "_individual-densities-histogram.png",
      dataDisagg.map(_.toDouble),
      binSize,
      "individual density [pax/m^2]",
      "Histogram of individual densities",
      opts
    )
    //println(results.flatMap(_.monitoredAreaIndividualDensity.map(_.tail).flatten))
    //println("individual density " + results.flatMap(_.monitoredAreaIndividualDensity.get.map(_.tail).flatten).stats)
    //println(computeQuantiles(Vector(65,70,75,80,85,90,95,97,99))(results.flatMap(_.monitoredAreaIndividualDensity.get.map(_.tail).flatten)))
    computeHistogramDataWithXValues(dataDisagg.map(_.toDouble), binSize, opts.xmin, opts.xmax).writeToCSV(config.getString("output.output_prefix") + "_individual-densities-hist_data.csv", rowNames = None, columnNames = Some(Vector("x", "y")))

  }

  // computes statistics on travel times and writes them
  if (config.getBoolean("output.write_tt_stats")) {

    // writes stattistcs about each run
    results.map(r => r.tt.map(_._3).stats).writeToCSV(config.getString("output.output_prefix") + "_travel_times_stats.csv", rowNames = None, columnNames = Some(Vector("size", "mean", "variance", "median", "min", "max")))

    // creates hist data for all TT aggregated together
    val data: Seq[Double] = results.flatMap(_.tt.map(_._3)).cutOfAfterQuantile(99)
    val binSize = 1.0
    val opts = PlotOptions(xmin = Some(10), xmax = Some(50), ymax = Some(0.25), width = 700, height = 400)
    new Histogram(config.getString("output.output_prefix") + "_travel_times_hist.png", data, binSize, "travel times [s]", "Fixed separator", opts)
    computeHistogramDataWithXValues(data, binSize, opts.xmin, opts.xmax).writeToCSV(config.getString("output.output_prefix") + "_travel_times_hist_data.csv", rowNames = None, columnNames = Some(Vector("x", "y")))
    println(data.stats)
    //println(computeHistogramDataWithXValues(data, binSize, opts.xmin, opts.xmax))
    //println("tt " + results.flatMap(_.tt.map(_._3)).cutOfAfterQuantile(99).stats)
    //println(computeQuantiles(Vector(65,70,75,80,85,90,95,97,99))(results.flatMap(_.tt.map(_._3)).cutOfAfterQuantile(99)))
  }

  // Analyse pedestrian data like travel time and walking speed by departure time interval.
  //if (!config.getStringList("results-analysis.o_nodes").isEmpty && !config.getStringList("results-analysis.d_nodes").isEmpty) {
  //val ODPairsToAnalyse: Iterable[(String, String)] = config.getStringList("results-analysis.o_nodes").asScala.zip(config.getStringList("results-analysis.d_nodes").asScala).map(t => (t._1, t._2))



  //val ODGroupsToAnalyse: Seq[(Seq[String], Seq[String])] = Vector((Vector("top", "bottom"), Vector("left-top", "right-bottom")), (Vector("left-bottom", "right-top"), Vector("right-bottom", "left-top")))
  val ODGroupsToAnalyse: Seq[(Seq[String], Seq[String])] = Vector((Vector("top"), Vector("bottom")), (Vector("bottom"), Vector("top")))

  def makeStringODGroups(group: (Seq[String], Seq[String])): String = group._1.mkString("_") + "_TO_" + group._2.mkString("_")

  def findInterval(t: Double, times: Vector[BigDecimal]): Double = {
    times(times.indexWhere(_ > t)).toDouble
  }

  def pedWindows: Tuple5[String, String, Double, Double, Double] => Double = ped => findInterval(ped._4, (simulationStartTime.value to simulationEndTime.value by evaluationInterval.value).toVector)

  def pedFilter: Tuple5[String, String, Double, Double, Double] => Boolean = ped => true //ODPairsToAnalyse.exists(_ == (ped._1, ped._2))

  def pedData: Tuple5[String, String, Double, Double, Double] => Double = ped => ped._3

  val populationGrouped: Iterable[Vector[(String, String, Double, Double, Double)]] = ODGroupsToAnalyse.map(g => results.flatMap(_.tt).filter(tt => g._1.contains(tt._1) && g._2.contains(tt._2)))

  populationGrouped.zip(ODGroupsToAnalyse).map(subPop => {
    //val ttByIntervals: Map[Double, (Int, Double, Double,Double, Double, Double)] = subPop.aggregateMetricByTimeWindow(pedFilter, pedData, pedWindows)
    if (subPop._1.isEmpty) {
      println("Empty subPop: " + subPop._2)
    }
    else {
      val tt = subPop._1.map(_._3).cutOfAfterQuantile(99)
      new Histogram(config.getString("output.output_prefix") + makeStringODGroups(subPop._2) + "-travel-time.png", tt, 1.0, "travel time [s]", "Travel time for " + makeStringODGroups(subPop._2), PlotOptions(xmin = Some(10), xmax = Some(70), ymax = Some(0.35)))
      computeHistogramDataWithXValues(tt, 1.0, Some(10), Some(50)).writeToCSV(config.getString("output.output_prefix") + makeStringODGroups(subPop._2) + "-travel-time-hist-data.csv")
    }
    subPop._1.map(_._3).cutOfAfterQuantile(99).stats
  }).toVector.writeToCSV(config.getString("output.output_prefix") + "_statsByOD.csv", rowNames = Some(ODGroupsToAnalyse.map(makeStringODGroups)), columnNames = None)


  // ******************************************************************************************
  //                                  Processing for TRANS-FORM
  // ******************************************************************************************

  if (config.getBoolean("output.write_tt_4_transform")) {

    val stop2Vertex = readPTStop2GraphVertexMap(config.getString("files.zones_to_vertices_map"))

    def vertices2Stops(vertexID: VertexID): String = {
      val reversedMap: Map[String, String] = stop2Vertex.stop2Vertices.flatMap(kv => kv._2.map(v => v -> kv._1.toString))
      reversedMap.getOrElse(vertexID, vertexID.toString)
    }

    results.flatten(_.tt).computeTT4TRANSFORM(0.0.to(100.0).by(config.getDouble("output.write_tt_4_transform_quantile_interval")), simulationStartTime, simulationEndTime, config.getString("output.write_tt_4_transform_file_name"), vertices2Stops)
  }
}
