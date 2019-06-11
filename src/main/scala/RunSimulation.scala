
import com.typesafe.config.Config
import hubmodel.DES._
import hubmodel._
import hubmodel.demand.readDemandSets
import hubmodel.io.input.JSONReaders.ODGroup_JSON
import hubmodel.io.output.TRANSFORM.PopulationSummaryProcessingTRANSFORM
import hubmodel.ped.PedestrianNOMAD
import hubmodel.results.{ResultsContainerRead, ResultsContainerReadNew, ResultsContainerReadWithDemandSet, ResultsContainerReadWithDemandSetNew, readResults, readResultsJson}
import hubmodel.supply.graph.readPTStop2GraphVertexMap
import hubmodel.tools.Time
import myscala.math.stats.bootstrap.bootstrapMSE
import myscala.math.stats.{ComputeQuantiles, ComputeStats, computeBoxPlotData, computeQuantile}
import myscala.output.SeqExtension.SeqWriter
import myscala.output.SeqOfSeqExtensions.SeqOfSeqWriter
import myscala.output.SeqTuplesExtensions.SeqTuplesWriter
import play.api.libs.json.{JsError, JsSuccess, JsValue, Json}
import trackingdataanalysis.visualization.{Histogram, PlotOptions, ScatterPlot, computeHistogramDataWithXValues}

import scala.collection.GenIterable
import scala.io.BufferedSource


/**
  * Runs the simulations based on the configuration file. This configuration file contains all the details regarding
  * the eventual management strategies to use, the number of runs to perform, the simulation time steps and the
  * processing of the results. The keys steps for beaing able to run a simulation are the following:
  *
  */
object RunSimulation extends App with StrictLogging {


  // ******************************************************************************************
  //                    Read CLI arguments and validate parameters file
  // ******************************************************************************************

  logger.info("Reading config and preparing simulations")

  // parses the config file and checks if the output dir exists.
  val config: Config = parseConfigFile(args)


  // ******************************************************************************************
  //                        Prepare all simulations and runs them
  // ******************************************************************************************

  //
  val simulationStartTime: Time = Time(config.getDouble("sim.start_time"))
  val simulationEndTime: Time = Time(config.getDouble("sim.end_time"))

  val runSimulationsInParallel: Boolean = config.getBoolean("execution.parallel")
  val evaluationInterval: Time = Time(config.getDouble("sim.evaluate_dt"))

  val demandSets: Option[Seq[(String, String)]] = readDemandSets(config)

  val n: Int = computeNumberOfSimulations(config, demandSets)

  // Runs the simulations in parallel or sequential based on the config file.

  if (config.getBoolean("output.make_video")) {
    logger.info("Running simulation for video...")
    runSimulationWithVideo(config, if (demandSets.isDefined) {
      Some(demandSets.get.head._1)
    } else {
      None
    })
  }


  val range: GenIterable[Int] = getIteratorForSimulations(if (runSimulationsInParallel) {
    Some(config.getInt("execution.threads"))
  } else {
    None
  }, n)

  if (n > 0) {
    range.foreach(s => {
      val sim =
        if (demandSets.isDefined && config.getBoolean("sim.read_multiple_TF_demand_sets")) {
          createSimulation[PedestrianNOMAD](config, Some(demandSets.get(s - 1)._1), Some(demandSets.get(s - 1)._2))
        } else if (demandSets.isDefined && config.getBoolean("sim.read_multiple_demand_sets")) {
          createSimulation[PedestrianNOMAD](config, Some(demandSets.get(s - 1)._1))
        } else {
          createSimulation[PedestrianNOMAD](config)
        }

      val outputDir: String = if (config.getBoolean("sim.read_multiple_demand_sets") || config.getBoolean("sim.read_multiple_TF_demand_sets")) {
        config.getString("output.dir") + demandSets.get(s - 1)._1.split("\\.").head + "/"
      } else {
        config.getString("output.dir")
      }

      runAndWriteResults(
        sim,
        config.getString("output.output_prefix") + "_",
        outputDir,
        config.getBoolean("output.write_trajectories_as_VS"),
        config.getBoolean("output.write_trajectories_as_JSON"),
        config.getBoolean("output.write_tt_4_transform")
      )
      System.gc()
    })
  } else {
    logger.warn("No more simulations to run !")
  }


  // Reads intermediate results
  val results: Vector[ResultsContainerRead] = if (demandSets.isDefined) {
    readResults(config.getString("output.dir"), config.getString("output.output_prefix"), demandSets.get.map(_._1.split("\\.").head)).toVector
  } else {
    readResults(config.getString("output.dir"), config.getString("output.output_prefix")).toVector
  }

  val resultsJson: Vector[ResultsContainerReadNew] = if (demandSets.isDefined) {
    readResultsJson(config.getString("output.dir"), config.getString("output.output_prefix"), demandSets.get.map(_._1.split("\\.").head)).toVector
  } else {
    readResultsJson(config.getString("output.dir"), config.getString("output.output_prefix")).toVector
  }


  if (results.isEmpty) {
    throw new Exception("No result collections have been read !")
  }


  // Processing results
  logger.info("Processing results")

  // Collects then writes individual travel times to csv
  if (config.getBoolean("output.write_travel_times") && results.nonEmpty){
    results
      .map(r => r.tt.map(_._3))
      .writeToCSV(
        config.getString("output.output_prefix") + "_travel_times.csv",
        columnNames = Some(Vector.fill(results.size)("r").zipWithIndex.map(t => t._1 + t._2.toString)),
        rowNames = None
      )
  }

  // Collects then writes individual travel times with OD to csv
  /*if (config.getBoolean("output.write_travel_times")) results
    .zipWithIndex
    .flatMap(r => r._1.tt.map(p => (r._2, p._3, p._1, p._2)))
    .writeToCSV(
      config.getString("output.output_prefix") + "_travel_times_OD.csv",
      columnNames = Some(Vector("run", "travel_time", "origin_id", "destination_id")),
      rowNames = None
    )*/

  if (config.getBoolean("output.write_densities") && results.nonEmpty) {
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

  if (config.getBoolean("output.write_density_stats") && results.nonEmpty) {

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

    val binSize = 0.05
    val opts = PlotOptions(xmin = Some(0.5), xmax = Some(3), ymax = Some(0.15), width = 700, height = 400)
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

    val dataDisagg = results.map(_.monitoredAreaIndividualDensity.get.map(_._2))
    /*new Histogram(config.getString("output.output_prefix") + "_median-individual-densities-histogram.png",
      dataDisagg,
      binSize,
      "individual density [pax/m^2]",
      "Histogram of individual densities",
      opts
    )*/

    dataDisagg.map(_.statistics.mean).writeToCSV(config.getString("output.output_prefix") + "-mean-individual-densities-per-simulation.csv")

    (for (i <- Vector(50, 55, 60, 65, 70, 75, 80, 85, 90, 95)) yield {
      (i, computeBoxPlotData(dataDisagg.map(d => computeQuantile(i)(d).value.toDouble)).toCSV, 1)
    }).writeToCSV(config.getString("output.output_prefix") + "-individual-densities-boxplot-per-quantile.csv", rowNames = None, columnNames = Some(Vector("quantile", "mean", "median", "lq", "uq", "lw", "uw", "outliersize", "boxplotwidth")))

  }

  // computes statistics on travel times and writes them
  if (config.getBoolean("output.write_tt_stats") && results.nonEmpty) {

    // writes stattistcs about each run
    val statsPerRun = results.map(r => {
      r.tt.map(_._3)/*.cutOfAfterQuantile(99.9)*/.statistics
    })

    Vector((0.0, computeBoxPlotData(statsPerRun.map(_.median)).toCSV, 0.8)).writeToCSV(config.getString("output.output_prefix") + "-travel-time-median-boxplot.csv", rowNames = None, columnNames = Some(Vector("pos", "mean", "median", "lq", "uq", "lw", "uw", "outliersize", "boxplotwidth")))

    statsPerRun.writeToCSV(config.getString("output.output_prefix") + "_travel_times_stats.csv", rowNames = None, columnNames = Some(Vector("size", "mean", "variance", "median", "min", "max")))

    def mean(data: Seq[Double]): Double = {
      data.sum / data.size
    }

    println(bootstrapMSE(statsPerRun.map(_.median), mean))

    //println(statsPerRun.map(_.median).zipWithIndex.sortBy(_._1))

    val quant: Double = 75
    (for (i <- 1 to results.size by 2) yield {
      val mseResults = for (j <- 1 to 100) yield {
        bootstrapMSE(util.Random.shuffle(statsPerRun).take(i).map(_.median), mean)
      }
      (i, computeQuantile(quant)(mseResults.map(_.MSE)).value, computeQuantile(quant)(mseResults.map(r => r.MSE / r.parameter)).value)
    }).toVector.writeToCSV(config.getString("output.output_prefix") + "-" + quant.toString + "quant-MSE.csv", rowNames = None, columnNames = Some(Vector("n", "mse", "rmse")))


    // creates hist data for all TT aggregated together
    val data: Seq[Double] = results.flatMap(_.tt.map(_._3))
    val binSize = 1.0
    val opts = PlotOptions(xmin = Some(10), xmax = Some(50), ymax = Some(0.25), width = 700, height = 400)
    new Histogram(config.getString("output.output_prefix") + "_travel_times_hist.png", data, binSize, "travel times [s]", "Fixed separator", opts)
    computeHistogramDataWithXValues(data, binSize, opts.xmin, opts.xmax).writeToCSV(config.getString("output.output_prefix") + "_travel_times_hist_data.csv", rowNames = None, columnNames = Some(Vector("x", "y")))
  }

  // Writes the travel time distribution of each simulation two a csv file.
  if (config.getBoolean("output.travel-time.per-simulation-distributions") && results.nonEmpty) {
    val r: Seq[Seq[Double]] = (0.0 to 200.0 by 2.0) +: results.map(r => {
      val data = r.tt.map(_._3) //.cutOfAfterQuantile(99)
      computeHistogramDataWithXValues(data, 2.0, Some(0), Some(200), normalized = false).map(_._2)
    })
    r.writeToCSV(config.getString("output.output_prefix") + "_travel_times_distributions.csv")
  }

  // Writes the median travel times to a csv file
  if (config.getBoolean("output.travel-time.per-simulation-median") && results.nonEmpty) {
    results.map(r => r.tt.map(_._3).statistics.median).writeToCSV(config.getString("output.output_prefix") + "_median-travel-time-per-simulation.csv")
  }


  // reads the group json file
  lazy val odGroups = {
    val source: BufferedSource = scala.io.Source.fromFile(config.getString("output.OD-groups"))
    val input: JsValue = Json.parse(try source.mkString finally source.close)
    input.validate[Vector[ODGroup_JSON]] match {
      case s: JsSuccess[Vector[ODGroup_JSON]] => {
        s.get
      }
      case e: JsError => throw new Error("Error while parsing od groups: " + JsError.toJson(e).toString())
    }
  }

  // reverses the map and create a function to use it without re-creating it every time
  lazy val groupsReversed: ((String, String)) => String = odPair => {
    odGroups.flatMap(g => g.ods.map(od => (od.o, od.d) -> g.name)).toMap.getOrElse(odPair, "")
  }


  if (config.getBoolean("output.travel-time.per-simulation-median-by-OD") && results.nonEmpty) {

    // each group becomes one column. The columns are sorted alphabetically.
    val columnNames: Vector[String] = resultsJson.head.tt.groupBy(p => groupsReversed((p.o, p.d))).keys.toVector.sortBy(a => a)
    resultsJson
      .map(r => {
        val tmp = r.tt.groupBy(p => groupsReversed((p.o, p.d))).map(g => (g._1, g._2.map(_.tt).statistics.median))
        tmp ++ columnNames.filterNot(c => tmp.keys.toVector.contains(c)).map(n => (n, Double.NaN))
      }.toVector.sortBy(_._1).map(_._2))
      .transpose
      .writeToCSV(config.getString("output.output_prefix") + "_median-travel-time-per-simulation-by-OD.csv", rowNames = None, columnNames = Some(columnNames))

    resultsJson
      .flatMap(r => r.tt.groupBy(p => groupsReversed((p.o, p.d))).map(g => (g._1, g._2.map(_.tt).statistics.median)))
      .groupBy(_._1)
      .zipWithIndex
      .map(g => (g._2, g._1._1, computeBoxPlotData(g._1._2.map(_._2)).toCSV, 1.0)).toVector
      .writeToCSV(config.getString("output.output_prefix") + "_median-travel-time-per-simulation-by-OD-boxplot-data.csv", rowNames = None, columnNames = Some(Vector("pos", "name", "mean", "median", "lq", "uq", "lw", "uw", "outliersize", "boxplotwidth")))
  }

  if (config.getBoolean("output.travel-time.through-monitored-zones-by-OD") && resultsJson.nonEmpty) {
    val columnNames: Vector[String] = resultsJson.flatMap(r => r.tt.flatMap(p => p.ttThroughZones.map(ttZ => groupsReversed((p.o, p.d)) + "_" + ttZ.ID))).distinct.sorted
    resultsJson
      .map(r => r.tt.flatMap(p => p.ttThroughZones.map(ttZ => (p.o, p.d, ttZ.ID, ttZ.tt)))
        .groupBy(p => (groupsReversed((p._1, p._2)), p._3))
        .map(g => (g._1._1 + "_" + g._1._2, g._2.map(_._4).statistics.median))
        .toVector.sortBy(_._1).map(_._2))
      .transpose
      .writeToCSV(config.getString("output.output_prefix") + "_median-travel-time-through-zones-per-simulation-by-OD.csv", rowNames = None, columnNames = Some(columnNames))
  }

  if (demandSets.isDefined && config.getBoolean("output.travel-time.per-demand-set-median-distribution")) {
    val mediansPerDemand = resultsJson
      .asInstanceOf[Vector[ResultsContainerReadWithDemandSetNew]]
      .groupBy(_.demandFile)
      .mapValues(rr => rr.map(r => r.tt.map(_.tt).statistics.median))

    mediansPerDemand
      .map(r => computeHistogramDataWithXValues(r._2, 0.05, Some(40), Some(55)))
      .map(_.map(_._3)).toSeq
      .writeToCSV(config.getString("output.output_prefix") + "_travel_times_median_hist_data.csv", rowNames = None, columnNames = None)

    mediansPerDemand
      .map(r => (r._1, computeBoxPlotData(r._2).toCSV, 1.0)).toVector.sortBy(_._1)
      .writeToCSV(config.getString("output.output_prefix") + "-travel-time-medians-boxplots-per-demand-set.csv", rowNames = None, columnNames = Some(Vector("demandset", "mean", "median", "lq", "uq", "lw", "uw", "outliersize", "boxplotwidth")))
  }


  private val entranceTimes: Iterable[Time] = results.flatMap(_.tt.map(t => Time(t._4)))
  val ttMin: Time = entranceTimes.min
  val ttMax: Time = entranceTimes.max
  val binnedData: Iterable[Seq[(Int, Double, Double)]] = results.map(r => computeHistogramDataWithXValues(r.tt.map(_._4), 60, Some(math.floor(ttMin.value.toDouble / 60.0) * 60.0), Some(ttMax.value.toDouble), normalized = false))
  (binnedData.head.map(_._2) +: binnedData.map(_.map(_._3)).toVector).writeToCSV(config.getString("output.output_prefix") + "_entrance_times_distribution.csv")


  results.collect({ case f: ResultsContainerReadWithDemandSet => {
    f
  }
  })
    .groupBy(_.demandFile)
    .foreach(g => println((g._2.map(_.tt.map(_._3).cutOfAfterQuantile(99.5).statistics.median).statistics.median, g._2.map(_.tt.map(_._3).cutOfAfterQuantile(99.5).statistics.variance).statistics.median)))


  // Analyse pedestrian data like travel time and walking speed by departure time interval.
  //if (!config.getStringList("results-analysis.o_nodes").isEmpty && !config.getStringList("results-analysis.d_nodes").isEmpty) {
  //val ODPairsToAnalyse: Iterable[(String, String)] = config.getStringList("results-analysis.o_nodes").asScala.zip(config.getStringList("results-analysis.d_nodes").asScala).map(t => (t._1, t._2))


  /*if (config.getBoolean("output.analyze_od_groups") && results.nonEmpty) {
    //val ODGroupsToAnalyse: Seq[(Seq[String], Seq[String])] = Vector((Vector("top", "bottom"), Vector("left-top", "right-bottom")), (Vector("left-bottom", "right-top"), Vector("right-bottom", "left-top")))
    val ODGroupsToAnalyse: Seq[(Seq[String], Seq[String])] = Vector((Vector("top"), Vector("bottom")), (Vector("bottom"), Vector("top")))

    def makeStringODGroups(group: (Seq[String], Seq[String])): String = group._1.mkString("_") + "_TO_" + group._2.mkString("_")

    def findInterval(t: Double, times: Vector[BigDecimal]): Double = {
      times(times.indexWhere(_ > t)).toDouble
    }

    def pedWindows: Tuple5[String, String, Double, Double, Double] => Double = ped => findInterval(ped._4, (simulationStartTime.value to simulationEndTime.value by evaluationInterval.value).toVector)

    def pedFilter: Tuple5[String, String, Double, Double, Double] => Boolean = ped => true //ODPairsToAnalyse.exists(_ == (ped._1, ped._2))

    def pedData: Tuple5[String, String, Double, Double, Double] => Double = ped => ped._3

    val populationGrouped: Iterable[Vector[(String, String, Double, Double, Double, Double)]] = ODGroupsToAnalyse.map(g => results.flatMap(_.tt).filter(tt => g._1.contains(tt._1) && g._2.contains(tt._2)))

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
  }*/

  // ******************************************************************************************
  //                                  Processing for TRANS-FORM
  // ******************************************************************************************

  if (config.getBoolean("output.write_tt_4_transform") && results.nonEmpty) {

    val stop2Vertex = readPTStop2GraphVertexMap(config.getString("files.zones_to_vertices_map"))

    val resultsByOD = results
      .flatten(_.tt)
      .computeTT4TRANSFORM(0.0.to(100.0).by(config.getDouble("output.write_tt_4_transform_quantile_interval")), simulationStartTime, simulationEndTime, config.getString("output.write_tt_4_transform_file_name"), stop2Vertex)

    resultsByOD.map(r => (r._1 + "->" + r._2, r._3.stats)).toVector.sortBy(_._1).map(v => (v._1, v._2._1, v._2._2, v._2._3, v._2._4, v._2._5, v._2._6)).writeToCSV(config.getString("output.output_prefix") + "_walking_time_distributions_by_OD.csv")
  }
}
