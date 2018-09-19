
import com.typesafe.config.{Config, ConfigFactory}
import hubmodel.DES.SFGraphSimulator
import hubmodel._
import hubmodel.output.TRANSFORM.PopulationSummaryProcessingTRANSFORM
import hubmodel.ped.PedestrianSim
import hubmodel.results.PopulationSummaryProcessing
import myscala.math.stats.ComputeStats
import myscala.math.stats.ComputeQuantiles
import myscala.math.stats.computeQuantiles
import myscala.output.SeqOfSeqExtensions.SeqOfSeqWriter
import myscala.output.SeqTuplesExtensions.SeqTuplesWriter
import trackingdataanalysis.visualization.{Histogram, ScatterPlot}
import visualization.PlotOptions

import scala.collection.GenIterable
import scala.collection.JavaConverters._
import scala.collection.mutable.ArrayBuffer
import scala.collection.parallel.{ExecutionContextTaskSupport, ForkJoinTaskSupport}

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



  // ******************************************************************************************
  //                           Processes and writes results to CSV
  // ******************************************************************************************


  // Checks that the number of simulations to run is coherent
  /*if ( config.getInt("sim.nb_runs") == 0 && !config.getBoolean("output.make_video")) {
    throw new IllegalArgumentException("No simulation to run ! Check parameters in config file.")
  }*/

  val simulationStartTime: Time = Time(config.getDouble("sim.start_time"))
  val simulationEndTime: Time = Time(config.getDouble("sim.end_time"))
  val n: Int = config.getInt("sim.nb_runs")
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
  else { 1 to n }


  if (n > 0) {
      range.foreach(s => {
        val sim = createSimulation(config)
        runAndWriteResults(sim, "sim_results_", config.getString("output.dir"))
        System.gc()
      })
  } else {
      println("No more simulations to run !")
  }

  // Reads intermediate results
  val results: Vector[ResultsContainerRead] = readResults(config.getString("output.dir")).toVector


  // Processing results
  if (config.getBoolean("output.write_travel_times")) {

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
    if (config.getBoolean("output.write_travel_times")) results
      .zipWithIndex
      .flatMap(r => r._1.tt.map(p => (r._2, p._3, p._1, p._2)))
      .writeToCSV(
        config.getString("output.output_prefix") + "_travel_times_OD.csv",
        columnNames = Some(Vector("run", "travel_time", "origin_id", "destination_id")),
        rowNames = None)

  }

  if ( config.getBoolean("output.write_densities") ) {
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
            densityStatsPerTime(ii)._5, densityStatsPerTime(ii)._6) ++ densities.map(_(ii))
        }).transpose.writeToCSV(
          config.getString("output.output_prefix") + "_" + i + "_densities.csv",
          rowNames = Some(densityTimes.map(_.toString)),
          columnNames = Some(Vector("time", "size", "mean", "variance", "median","min", "max") ++ Vector.fill(results.size)("r").zipWithIndex.map(t => t._1 + t._2.toString))
        )
      }
    }
  }

  if (config.getBoolean("output.write_density_stats")) {
    val targetDensityRange = BigDecimal(0.0) to BigDecimal(3.5) by BigDecimal(0.25)

    val individualDensityAboveThreshold: Map[BigDecimal, Vector[(Double, Int)]] = targetDensityRange.map(rho => rho -> {
      results.flatMap(_.monitoredAreaIndividualDensity.get.groupBy(_.head).map(v => v._1 -> v._2.flatMap(d => d.tail).count(_ > rho.doubleValue())))
    }).toMap.map(v => v._1 -> v._2.sortBy(_._1))

    individualDensityAboveThreshold.toVector.sortBy(_._1).map(_._2.map(_._2)).writeToCSV(config.getString("output.output_prefix") + "-pax-above-target.csv", rowNames=None, columnNames=Some(targetDensityRange.map(_.toString)))

    new ScatterPlot(config.getString("output.output_prefix") + "_time-pax-above-threshold.png",
      individualDensityAboveThreshold(2.0).map(_._1),
      individualDensityAboveThreshold(2.0).map(_._2.toDouble),
      "time [s]",
      "# individuals with density above threshold",
      PlotOptions(ymax=Some(80))
    )

    new Histogram(config.getString("output.output_prefix") + "_density-histogram.png",
      results.flatMap(_.monitoredAreaDensity.get._2.flatten),
      0.1,
      "densities [pax/m^2]",
      "Histogram of densities measured in area",
      PlotOptions(xmin=Some(0), xmax=Some(6.0), ymax=Some(0.10))
    )
    println("density " + results.flatMap(_.monitoredAreaDensity.get._2.flatten).stats)
    println(computeQuantiles(Vector(65,70,75,80,85,90,95,97,99))(results.flatMap(_.monitoredAreaDensity.get._2.flatten)))

    new Histogram(config.getString("output.output_prefix") + "_individual-densities-histogram.png",
      results.flatMap(_.monitoredAreaIndividualDensity.get.map(_.tail).flatten),
      0.1,
      "individual density [pax/m^2]",
      "Histogram of individual densities",
      PlotOptions(xmin=Some(0), xmax=Some(6.0), ymax=Some(0.04))
    )
    //println(results.flatMap(_.monitoredAreaIndividualDensity.map(_.tail).flatten))
    println("individual density " + results.flatMap(_.monitoredAreaIndividualDensity.get.map(_.tail).flatten).stats)
    println(computeQuantiles(Vector(65,70,75,80,85,90,95,97,99))(results.flatMap(_.monitoredAreaIndividualDensity.get.map(_.tail).flatten)))

  }

  // computes statistics on travel times and writes them
  if (config.getBoolean("output.write_tt_stats")) {
    results.map(r => r.tt.map(_._3).stats).writeToCSV(config.getString("output.output_prefix") + "_travel_times_stats.csv", rowNames = None, columnNames = Some(Vector("size", "mean", "variance", "median", "min", "max")))
    new Histogram(config.getString("output.output_prefix") + "_travel_times_hist.png", results.flatMap(_.tt.map(_._3)), 2.0, "travel times [s]", "Histogram of travel times", PlotOptions(xmin=Some(20), xmax=Some(80.0), ymax=Some(0.25)))
    println("tt " + results.flatMap(_.tt.map(_._3)).cutOfAfterQuantile(99).stats)
    println(computeQuantiles(Vector(65,70,75,80,85,90,95,97,99))(results.flatMap(_.tt.map(_._3)).cutOfAfterQuantile(99)))
  }

  // Analyse pedestrian data like travel time and walking speed by departure time interval.
  if (!config.getStringList("results-analysis.o_nodes").isEmpty && !config.getStringList("results-analysis.d_nodes").isEmpty)
  {
    val ODPairsToAnalyse: Iterable[(String, String)] = config.getStringList("results-analysis.o_nodes").asScala.zip(config.getStringList("results-analysis.d_nodes").asScala).map(t => (t._1, t._2))

    def findInterval(t: Double, times: Vector[Double]): Double = {
      println(t)
      times(times.indexWhere(_ > t))
    }

    def pedWindows: Tuple5[String, String, Double, Double, Double] => Double = ped => findInterval(ped._4, (simulationStartTime.value to simulationEndTime.value by evaluationInterval.value).toVector)

    def pedFilter: Tuple5[String, String, Double, Double, Double] => Boolean = ped => true //ODPairsToAnalyse.exists(_ == (ped._1, ped._2))

    def pedData: Tuple5[String, String, Double, Double, Double] => Double = ped => ped._3

    val ttByIntervals: IndexedSeq[IndexedSeq[(Double, Double)]] = results.map(r => {
      val res = r.tt.aggregateMetricByTimeWindow(pedFilter, pedData, pedWindows)
      (res.map( r => (r._1, r._2._2)).toVector ++ (simulationStartTime.value to simulationEndTime.value by evaluationInterval.value).filterNot(res.keySet.contains(_)).map(t => (t, Double.NaN))).sortBy(_._1)//
    })

    val ttStats: IndexedSeq[(Double, (Int, Double, Double, Double, Double, Double))] = (for (i <- simulationStartTime.value to simulationEndTime.value by evaluationInterval.value) yield {
      i -> (for (j <- results.indices if ttByIntervals(j).exists(_._1 == i)) yield { ttByIntervals(j).find(_._1 == i).get._2}).filterNot(_.isNaN).stats
    }).sortBy(_._1)

    (ttStats.map(_._1) +: ttStats.map(d => d._2._1) +: ttStats.map(d => d._2._2) +: ttStats.map(d => d._2._3) +: ttStats.map(d => d._2._4) +: ttByIntervals.map(_.map(_._2))).writeToCSV(
      config.getString("output.output_prefix") + "-mean-travel-time-per-time-interval.csv",
      rowNames = None, //Some((simulationStartTime.value to simulationEndTime.value by 5.0).map(_.toString)),
      columnNames = Some(Vector("time", "size", "mean", "variance", "median") ++ Vector.fill(results.size)("r").zipWithIndex.map(t => t._1 + t._2.toString))
    )

    new Histogram(config.getString("output.output_prefix") + "-travel-time-per-time-interval-histogram.png", ttByIntervals.flatMap(_.map(_._2)).filterNot(_.isNaN), 1.0,"travel time [s]", "Travel time per departure interval histogram", PlotOptions(xmin=Some(22), xmax=Some(40), ymax=Some(0.35)))
    //println("tt by departurew times " + ttByIntervals.flatMap(_.map(_._2)).filterNot(_.isNaN).stats, computeQuantile(75)(ttByIntervals.flatMap(_.map(_._2)).filterNot(_.isNaN)), computeQuantile(85)(ttByIntervals.flatMap(_.map(_._2)).filterNot(_.isNaN)),  computeQuantile(95)(ttByIntervals.flatMap(_.map(_._2)).filterNot(_.isNaN)), computeQuantile(97.5)(ttByIntervals.flatMap(_.map(_._2)).filterNot(_.isNaN)), computeQuantile(99)(ttByIntervals.flatMap(_.map(_._2)).filterNot(_.isNaN)))
  }

  // ******************************************************************************************
  //                                  Processing for TRANS-FORM
  // ******************************************************************************************

  if (config.getBoolean("output.write_tt_4_transform")) {
    results.flatten(_.tt).computeTT4TRANSFORM(0.0.to(100.0).by(config.getDouble("output.write_tt_4_transform_quantile_interval")), simulationStartTime, simulationEndTime, config.getString("output.write_tt_4_transform_file_name"))
  }
}

/*
,

 */