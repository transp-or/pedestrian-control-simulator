
import hubmodel.DES._
import hubmodel._
import hubmodel.demand.{DemandData, DemandSet, readDemandSets}
import hubmodel.io.input.JSONReaders.ODGroup_JSON
import hubmodel.io.output.TRANSFORM.{PopulationProcessingTRANSFORM, PopulationSummaryProcessingTRANSFORM}
import hubmodel.prediction.{AMWFlowsFromEmpiricalData, AMWFlowsFromGroundTruthProcessor}
import results.{ResultsContainerReadNew, ResultsContainerReadWithDemandSetNew, readResultsJson}
import hubmodel.supply.graph.readPTStop2GraphVertexMap
import tools.Time
import myscala.math.stats.bootstrap.bootstrapMSE
import myscala.math.stats.{ComputeQuantiles, ComputeStats, computeBoxPlotData, computeQuantile}
import myscala.output.SeqExtension.SeqWriter
import myscala.output.SeqOfSeqExtensions.SeqOfSeqWriter
import myscala.output.SeqTuplesExtensions.SeqTuplesWriter
import play.api.libs.json.{JsError, JsSuccess, JsValue, Json}
import trackingdataanalysis.pedtrack.ZoneProcessingNew
import trackingdataanalysis.visualization.{Histogram, PlotOptions, ScatterPlot, computeHistogramDataWithXValues}

import scala.collection.parallel.immutable.{ParSeq, ParVector}
import scala.collection.parallel.CollectionConverters._
import scala.collection.parallel.ForkJoinTaskSupport
import scala.io.BufferedSource
import com.typesafe.config.Config


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

  val simulationStartTime: Time = Time(config.getDouble("sim.start_time"))
  val simulationEndTime: Time = Time(config.getDouble("sim.end_time"))

  val runSimulationsInParallel: Boolean = config.getBoolean("execution.parallel")
  val evaluationInterval: Time = Time(config.getDouble("sim.evaluate_dt"))

  val demandSets: Option[Seq[DemandData]] = if (config.getBoolean("files.multiple_demand_sets_TF") || config.getBoolean("files.multiple_demand_sets") && config.getBoolean("sim.use_disaggregate_demand")) {
    readDemandSets(config)
  } else {
    None
  }

  val n: Int = computeNumberOfSimulations(config, demandSets)
  val threads: Int = math.min(n, config.getInt("execution.threads"))


  if (config.getBoolean("output.make_video")) {
    logger.info("Running simulation for video...")
    runSimulationWithVideo(config, demandSets.map(_.head))
  }

  // Runs the simulations in parallel or sequential based on the config.
  demandSets match {
    case Some(ds) if runSimulationsInParallel => {
      val parallelRuns: ParSeq[DemandData] = ds.par
      parallelRuns.tasksupport = new ForkJoinTaskSupport(new java.util.concurrent.ForkJoinPool(math.min(n, threads)))
      parallelRuns.foreach(s => { createRunWriteSimulation(Some(s), config) })
    }
    case Some(ds) if !runSimulationsInParallel => {
      Vector.range(0, n).foreach(s => {createRunWriteSimulation(Some(demandSets.get(s)), config)})
    }
    case None if n > 0 && runSimulationsInParallel => {
      val parallelRuns: ParVector[Int] = Vector.range(0, n).par
      parallelRuns.tasksupport = new ForkJoinTaskSupport(new java.util.concurrent.ForkJoinPool(math.min(n, threads)))
      parallelRuns.foreach(_ => createRunWriteSimulation(None, config))
    }
    case None if n > 0 && !runSimulationsInParallel => {
      Vector.range(0, n).foreach(s => {createRunWriteSimulation(None, config)})
    }
    case _ => {
      logger.warn("No simulations to run !")
    }
  }

  // Reads intermediate results
  /*val results: Vector[ResultsContainerRead] = if (demandSets.isDefined) {
    readResults(config.getString("output.dir") + demandSets.get.head.dir.getFileName + "/", config.getString("output.output_prefix"), demandSets.get.map(_.flowFile.getFileName.toString.replace(".json", "") + "/")).toVector
  } else {
    readResults(config.getString("output.dir"), config.getString("output.output_prefix")).toVector
  }*/

  val resultsJson: Vector[ResultsContainerReadNew] = if (demandSets.isDefined) {
    readResultsJson(config.getString("output.dir") + demandSets.get.head.dir.getFileName + "/", config.getString("output.output_prefix"), demandSets.get.map(_.flowFile.getFileName.toString.replace(".json", "") + "/")).toVector
  } else {
    readResultsJson(config.getString("output.dir"), config.getString("output.output_prefix")).toVector
  }


  if (resultsJson.nonEmpty) {


    // Processing results
    logger.info("Processing results")


    // Collects then writes individual travel times with OD to csv
    /*if (config.getBoolean("output.write_travel_times")) results
    .zipWithIndex
    .flatMap(r => r._1.tt.map(p => (r._2, p._3, p._1, p._2)))
    .writeToCSV(
      config.getString("output.output_prefix") + "_travel_times_OD.csv",
      columnNames = Some(Vector("run", "travel_time", "origin_id", "destination_id")),
      rowNames = None
    )*/

    /*if (config.getBoolean("output.write_densities") && resultsJson.nonEmpty) {

      // Collects times at which densities where measured
      val densityTimes: Vector[Time] = resultsJson.head.monitoredAreaDensity.get.map(_._2.aggregateMeasurement.map(t => Time(t._1)))

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
    }*/

    /*if (config.getBoolean("output.write_density_stats") && results.nonEmpty) {

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

    }*/

    if (config.getBoolean("output.density.individual-75") && resultsJson.nonEmpty) {
      // writes the applied data to the same csv file
      val individualDensitiesQuantile: Vector[(String, Vector[Double], String, Vector[Double])] = resultsJson
        .filter(_.monitoredAreaDensity.isDefined)
        .flatMap(v => v.monitoredAreaDensity.get.toVector.map(vv => (v.id, vv._1, vv._2.disaggregateMeasurements)))
        .sortBy(s => (s._1, s._2))
        .map(d => (d._1 + "_" + d._2 + "_t", d._3.map(v => v._1), d._1 + "_" + d._2 + "_d", d._3.map(v => if (v._2.isEmpty) {Double.NaN} else {computeQuantile(75)(v._2).value})))

      individualDensitiesQuantile.flatMap(d => Vector(d._2, d._4)).writeToCSV(config.getString("output.output_prefix") + "_individual_density_75.csv", rowNames=None, columnNames = Some(individualDensitiesQuantile.flatMap(d => Vector(d._1, d._3))))

      val averageDensities: Vector[(String, Vector[(Double, Double, Double, Double)])] = resultsJson
        .filter(_.monitoredAreaDensity.isDefined)
        .flatMap(_.monitoredAreaDensity.get)
        .groupBy(_._1)
        .map(kv => (kv._1, kv._2
            .flatMap(data => data._2.disaggregateMeasurements.map(v => (v._1, if (v._2.isEmpty) {0.0} else {computeQuantile(75)(v._2).value})))
            .groupBy(_._1).toVector.map(d =>  (d._1, d._2.map(_._2).sum / d._2.size.toDouble, computeQuantile(25)(d._2.map(_._2)).value, computeQuantile(75)(d._2.map(_._2)).value)).sortBy(_._1)))
        .toVector
        .sortBy(_._1)


      val averageDensitiesHeaders = averageDensities.flatMap(v => Vector(v._1 + "_t", v._1 + "_s", v._1 + "_lq", v._1 + "_uq"))

      averageDensities
        .flatMap(m => Vector(m._2.map(_._1), m._2.map(_._2), m._2.map(_._3), m._2.map(_._4)))
        .writeToCSV(config.getString("output.output_prefix") + "_average-individual_density_75.csv", columnNames = Some(averageDensitiesHeaders), rowNames = None)
    }

    if (config.getBoolean("output.density.mean-individual-75-integral") && resultsJson.nonEmpty) {
      val densityIntegrals: Vector[Double] = resultsJson
        .filter(_.monitoredAreaDensity.isDefined)
        .map(d => d.monitoredAreaDensity.get.map(d => d._2.integratedIndividualDensity).sum)

      Vector(computeBoxPlotData(densityIntegrals).toCSV)
        .writeToCSV(config.getString("output.output_prefix") +"_mean-density-individual-75-integral-boxplot.csv")

    }

    // computes statistics on travel times and writes them
    if (config.getBoolean("output.write_tt_stats") && resultsJson.nonEmpty) {

      // writes stattistcs about each run
      val statsPerRun = resultsJson.map(r => {
        r.tt.map(_.tt).statistics
      })

      Vector((0.0, computeBoxPlotData(statsPerRun.map(_.mean)).toCSV, 0.8)).writeToCSV(config.getString("output.output_prefix") + "-travel-time-mean-boxplot.csv", rowNames = None, columnNames = Some(Vector("pos", "mean", "median", "lq", "uq", "lw", "uw", "outliersize", "boxplotwidth")))

      statsPerRun.writeToCSV(config.getString("output.output_prefix") + "_travel_times_stats.csv", rowNames = None, columnNames = Some(Vector("size", "mean", "variance", "median", "min", "max")))

      def mean(data: Seq[Double]): Double = {
        data.sum / data.size
      }

      println(bootstrapMSE(statsPerRun.map(_.median), mean))

      //println(statsPerRun.map(_.median).zipWithIndex.sortBy(_._1))

      val quant: Double = 75
      (for (i <- 1 to resultsJson.size by 2) yield {
        val mseResults = for (j <- 1 to 100) yield {
          bootstrapMSE(util.Random.shuffle(statsPerRun).take(i).map(_.median), mean)
        }
        (i, computeQuantile(quant)(mseResults.map(_.MSE)).value, computeQuantile(quant)(mseResults.map(r => r.MSE / r.parameter)).value)
      }).toVector.writeToCSV(config.getString("output.output_prefix") + "-" + quant.toString + "quant-MSE.csv", rowNames = None, columnNames = Some(Vector("n", "mse", "rmse")))


      // Collects then writes individual travel times to csv
      if (config.getBoolean("output.travel-time.disaggregate") && resultsJson.nonEmpty) {
        resultsJson
          .map(r => r.tt.map(_.tt))
          .writeToCSV(
            config.getString("output.output_prefix") + "_travel_times.csv",
            columnNames = Some(Vector.fill(resultsJson.size)("r").zipWithIndex.map(t => t._1 + t._2.toString)),
            rowNames = None
          )
      }

      // creates hist data for all TT aggregated together
      val data: Seq[Double] = resultsJson.flatMap(_.tt.map(_.tt))
      val binSize = 1.0
      val opts = PlotOptions(xmin = Some(10), xmax = Some(50), ymax = Some(0.25), width = 700, height = 400)
      new Histogram(config.getString("output.output_prefix") + "_travel_times_hist.png", data, binSize, "travel times [s]", "Fixed separator", opts)
      computeHistogramDataWithXValues(data, binSize, opts.xmin, opts.xmax).writeToCSV(config.getString("output.output_prefix") + "_travel_times_hist_data.csv", rowNames = None, columnNames = Some(Vector("x", "y")))
    }

    // Writes the travel time distribution of each simulation two a csv file.
    if (config.getBoolean("output.travel-time.per-simulation-distributions") && resultsJson.nonEmpty) {
      val r: collection.immutable.Seq[collection.immutable.Seq[Double]] = (BigDecimal(0.0) to BigDecimal(200.0) by BigDecimal(2.0)).map(_.toDouble) +: resultsJson.map(r => {
        val data = r.tt.map(_.tt) //.cutOfAfterQuantile(99)
        computeHistogramDataWithXValues(data, 2.0, Some(0), Some(200), normalized = false).map(_._2)
      })
      r.writeToCSV(config.getString("output.output_prefix") + "_travel_times_distributions.csv")
    }

    // Writes the median travel times to a csv file
    if (config.getBoolean("output.travel-time.per-simulation-median") && resultsJson.nonEmpty) {
      resultsJson.map(r => r.tt.map(_.tt).statistics.median).writeToCSV(config.getString("output.output_prefix") + "_median-travel-time-per-simulation.csv")
    }

    // Writes the box plot of the mean travel per simulation to a csv file
    if (config.getBoolean("output.travel-time.per-simulation-mean-boxplot") && resultsJson.nonEmpty) {
      resultsJson.map(r => (computeBoxPlotData(r.tt.map(_.tt)).toCSV, 1.0))
        .writeToCSV(config.getString("output.output_prefix") + "_mean-travel-time-per-simulation-boxplot-data.csv", rowNames = None, columnNames = Some(Vector("mean", "median", "lq", "uq", "lw", "uw", "outliersize", "boxplotwidth")))
    }

    // Writes the box plot of the mean travel per simulation to a csv file
    if (config.getBoolean("output.travel-time.mean-boxplot") && resultsJson.nonEmpty) {
      Vector((computeBoxPlotData(resultsJson.map(r => r.tt.map(_.tt).statistics.mean)).toCSV, 1.0))
        .writeToCSV(config.getString("output.output_prefix") + "_mean-travel-time-boxplot-data.csv", rowNames = None, columnNames = Some(Vector("mean", "median", "lq", "uq", "lw", "uw", "outliersize", "boxplotwidth")))
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


    if (config.getBoolean("output.travel-time.per-simulation-median-by-OD-groups") && resultsJson.nonEmpty) {

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

    // write travel time mean by all existing OD pairs.
    if (config.getBoolean("output.travel-time.per-simulation-mean-by-OD") && resultsJson.nonEmpty) {

      // each od pair becomes one column. The columns are sorted alphabetically.
      val columnNames: Vector[String] = resultsJson.head.tt.groupBy(p => p.o + "->" +  p.d).keys.toVector.sorted

      // writes the mean of each od pair to csv file
      /*resultsJson
        .map(r => {
          val tmp: Map[String, Double] = r.tt.groupBy(p => p.o + "->" +  p.d).map(g => (g._1, g._2.map(_.tt).statistics.mean))
          tmp ++ columnNames.filterNot(c => tmp.keys.toVector.contains(c)).map(n => (n, Double.NaN))
        }.toVector.sortBy(_._1).map(_._2))
        .transpose
        .writeToCSV(config.getString("output.output_prefix") + "_mean-travel-time-per-simulation-by-OD.csv", rowNames = None, columnNames = Some(columnNames))*/

      // writes the data to create a boxplot in tikz to a csv file
      resultsJson
        .flatMap(r => r.tt.groupBy(p => p.o + "->" +  p.d).toVector.map(g => (g._1, g._2.map(_.tt))))
        .groupBy(_._1).toVector.map(g => g._1 -> g._2.flatMap(_._2))
        .sortBy(_._1)
        .zipWithIndex
        .map(g =>  (g._2, g._1._1, computeBoxPlotData(g._1._2).toCSV, 1.0)).toVector
        .writeToCSV(config.getString("output.output_prefix") + "_mean-travel-time-per-simulation-by-OD-boxplot-data.csv", rowNames = None, columnNames = Some(Vector("pos", "name", "mean", "median", "lq", "uq", "lw", "uw", "outliersize", "boxplotwidth")))
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


    val entranceTimes: Iterable[Time] =
      resultsJson.flatMap(_.tt.map(t => Time(t.entry)))
    val ttMin: Time = entranceTimes.min
    val ttMax: Time = entranceTimes.max
    val binnedData: Iterable[Seq[(Int, Double, Double)]] = resultsJson.map(r => computeHistogramDataWithXValues(r.tt.map(_.entry), 60, Some(math.floor(ttMin.value.toDouble / 60.0) * 60.0), Some(ttMax.value.toDouble), normalized = false))
    (binnedData.head.map(_._2) +: binnedData.map(_.map(_._3)).toVector).writeToCSV(config.getString("output.output_prefix") + "_entrance_times_distribution.csv")


    resultsJson.collect({ case f: ResultsContainerReadWithDemandSetNew => {
      f
    }
    })
      .groupBy(_.demandFile)
      .foreach(g => println((g._2.map(_.tt.map(_.tt).cutOfAfterQuantile(99.5).statistics.median).statistics.median, g._2.map(_.tt.map(_.tt).cutOfAfterQuantile(99.5).statistics.variance).statistics.median)))


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
    //                                        AMW data
    // ******************************************************************************************

    if (config.getBoolean("output.amws.control-policy")) {
      // writes the applied data to the same csv file
      val appliedSpeeds: Vector[(String, Vector[Double], String, Vector[Double])] = resultsJson
        .sortBy(_.id)
        .filter(_.amwData.isDefined)
        .flatMap(v => v.amwData.get.map(vv => (v.id, vv)))
        .map(w => ((w._1 + "_" + w._2.name + "_t", w._2.appliedPolicy.map(_._1), w._1 + "_" + w._2.name + "_s", w._2.appliedPolicy.map(_._2))))

      appliedSpeeds.flatMap(d => Vector(d._2, d._4)).writeToCSV(config.getString("output.output_prefix") + "_applied_amw_speeds.csv", rowNames=None, columnNames = Some(appliedSpeeds.flatMap(d => Vector(d._1, d._3))))

      val averageAMWSpeeds: Vector[(String, Vector[(Double, Double, Double, Double)])] = resultsJson
        .filter(_.amwData.isDefined)
        .flatMap(_.amwData.get)
        .groupBy(_.name)
        .map(kv => (kv._1, kv._2.flatMap(_.appliedPolicy).groupBy(_._1).toVector.map(speeds => (speeds._1, speeds._2.map(_._2).sum / speeds._2.size.toDouble, computeQuantile(25)(speeds._2.map(_._2)).value, computeQuantile(75)(speeds._2.map(_._2)).value)).sortBy(_._1)))
        .toVector
        .sortBy(_._1)


        val averageSpeedHeaders = averageAMWSpeeds.flatMap(s => Vector(s._1 + "_t", s._1 + "_s", s._1 + "_lq", s._1 + "_uq"))

      averageAMWSpeeds
        .flatMap(m => Vector(m._2.map(_._1), m._2.map(_._2), m._2.map(_._3), m._2.map(_._4)))
        .writeToCSV(config.getString("output.output_prefix") + "_amw-applied-average-speed.csv", columnNames = Some(averageSpeedHeaders), rowNames = None)



      // writes the expected data to separate csv files
      val expectedSpeeds: Map[(String, String), Vector[(String, Vector[Double], String, Vector[Double])]] = resultsJson
        .filter(_.amwData.isDefined)
        .flatMap(_.amwData.get)
        .map(w => (w.name, w.id) -> w.expectedPolicy.map(e => (w.name + "_t", e.map(_._1), w.name + "_s", e.map(_._2)))).toMap

      expectedSpeeds.foreach(e => {
        val headers: Vector[String] = e._2.flatMap(d => Vector(d._1, d._3))
        e._2.flatMap(d => Vector(d._2, d._4)).writeToCSV(config.getString("output.output_prefix") + "_expected_speeds_for_" + e._1._1 + "_" + e._1._2 + ".csv", rowNames=None, columnNames = Some(headers))
      })
 }

    // ******************************************************************************************
    //                                    Route choice data
    // ******************************************************************************************

    if (resultsJson.nonEmpty && config.getBoolean("output.routes-usage")) {
      val totalNumberPeds: Int = resultsJson.map(_.tt.size).sum

      resultsJson
        .flatMap(r => r.tt.collect{case p if p.exit.isDefined => Vector(p.route.head.node, p.route.last.node )/*p.route.map(_.node).distinct*/.mkString("-")})
        .groupBy(g => g)
        .view
        .mapValues(v => (v.size.toDouble / totalNumberPeds, v.size))
        .to(Map)
        .map(v => (v._1, v._2._1))
        .toVector
        .sortBy(_._1)
        .writeToCSV(config.getString("output.output_prefix") + "_routes_usage" + ".csv", rowNames = None, columnNames = Some(Vector("route", "fraction")))
    }


    // ******************************************************************************************
    //                                    Pedestrian flows
    // ******************************************************************************************

    if (config.getBoolean("output.flows")) {
      val interval: Int = 10
      val intervals: Vector[Time] = 27600.to(27900).by(interval).map(v => Time(v.toDouble)).toVector
      //val zoneProcessor: ZoneProcessingNew = new ZoneProcessingNew("E:\\PhD\\hub-simulator\\piw-corridor\\graph.json")
      val amw1Routes = Vector(Vector("bc1","c1","dc1"), Vector("bc2","c2","dc2"), Vector("amw11","amw12"))/*.map(r => r.map(zoneProcessor.vertices))*/
      val amw2Routes = Vector(Vector("amw21", "amw22"), Vector("d15","e15"), Vector("d15","e2"), Vector("d2","e2"), Vector("d2","e15"))/*.map(r => r.map(zoneProcessor.vertices))*/

      //val amwRoutes: Map[String, Vector[Vector[String]]] =  Map("amw1p" -> amw1Routes, "amw2p" -> amw2Routes, "amw1n" -> amw1Routes.map(_.reverse), "amw2n" -> amw2Routes.map(_.reverse))
      //val amwRoutes: Map[String, Vector[Vector[String]]] =  Map("j1" -> Vector(Vector("11a","11b"), Vector("12a", "12b"), Vector("14","14a"), Vector("13","13a")), "j34" -> Vector(Vector("9a", "9b"), Vector("10a", "10b")), "j56" -> Vector(Vector("8a", "8b"), Vector("7a", "7b")), "j78" -> Vector(Vector("6a", "6b"), Vector("5a", "5b"), Vector("a", "b1"), Vector("a", "b15"), Vector("a", "b2")))
      val amwRoutes: Map[String, Vector[Vector[String]]] =  Map("amw1p" -> Vector(Vector("b", "c", "d")), "amw1n" -> Vector(Vector("d", "c", "b")), "amw2p" -> Vector(Vector("d", "e")), "amw2n" -> Vector(Vector("e", "d")))

      val amwFlows: Vector[((String, String), Vector[(Time, Int)])] = resultsJson.flatMap(r => {
     r.tt.flatMap(p => {
          val rNames = p.route.map(_.node)
          amwRoutes
            .flatMap(rt => rt._2.map(r => rNames.indexOfSlice(r) match {case -1 => None case other => Some((rt._1, other))}))
            .collect{case Some(s) => s}
            .map(kv => (kv._1, p.route(kv._2), intervals.indexWhere(_.value > p.route(kv._2).t)-1))
        })
          .groupBy(g => (g._1, g._3))
          .map(g => (intervals(g._1._2), g._1._1, g._2.size))
          .groupBy(_._2)
          .map(g => (r.id, g._1) -> g._2.map(gg => (gg._1, gg._3)).toVector.sortBy(_._1))
          .toVector
      }).sortBy(_._1)

      val headers: Vector[String] = amwFlows.flatMap(h => Vector(h._1._1 + "_" + h._1._2 + "_t", h._1._1 + "_" + h._1._2 + "_f"))

      val tmp: Vector[Vector[Double]] = amwFlows.map(_._2)
        .flatMap(v => Vector(v.map(_._1.value.toDouble), v.map(_._2.toDouble)))

      tmp.writeToCSV(config.getString("output.output_prefix") + "_simplified-flows.csv", columnNames = Some(headers), rowNames = None)

      val averageFlows = amwFlows.groupBy(g => g._1._2)
        .toVector
        .map(m => (m._1, m._2.flatMap(n => n._2).groupBy(_._1).map(g => (g._1, g._2.map(_._2).sum.toDouble / (interval * g._2.size.toDouble), computeQuantile(25)(g._2.map(_._2)).value/interval.toDouble, computeQuantile(75)(g._2.map(_._2)).value/interval.toDouble)).toVector.sortBy(_._1)))
        .sortBy(_._1)

      val headersAverage = averageFlows.flatMap(m => Vector(m._1 + "_t", m._1 + "_f", m._1 + "_lq", m._1 + "_uq"))

      averageFlows.flatMap(m => Vector(m._2.map(_._1), m._2.map(_._2), m._2.map(_._3), m._2.map(_._4)))
        .writeToCSV(config.getString("output.output_prefix") + "_simplified-average-flows.csv", columnNames = Some(headersAverage), rowNames = None)
    }

    // ******************************************************************************************
    //                                  Processing for TRANS-FORM
    // ******************************************************************************************

    /*if (config.getBoolean("output.write_tt_4_transform") && resultsJson.nonEmpty) {

      val stop2Vertex = readPTStop2GraphVertexMap(config.getString("files.zones_to_vertices_map"))

      val resultsByOD = resultsJson
        .flatMap(_.tt)
        .computeTT4TRANSFORM(BigDecimal(0.0).to(BigDecimal(100.0)).by(config.getDouble("output.write_tt_4_transform_quantile_interval")), simulationStartTime, simulationEndTime, config.getString("output.write_tt_4_transform_file_name"), stop2Vertex)

      resultsByOD.map(r => (r._1 + "->" + r._2, r._3.stats)).toVector.sortBy(_._1).map(v => (v._1, v._2._1, v._2._2, v._2._3, v._2._4, v._2._5, v._2._6)).writeToCSV(config.getString("output.output_prefix") + "_walking_time_distributions_by_OD.csv")
    }*/
  }
}
