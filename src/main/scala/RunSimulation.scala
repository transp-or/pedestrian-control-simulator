
import com.typesafe.config.{Config, ConfigFactory}
import hubmodel.DES.SFGraphSimulator
import hubmodel._
import hubmodel.output.TRANSFORM.PopulationProcessingTRANSFORM
import hubmodel.ped.PedestrianSim
import hubmodel.results.PopulationProcessing
import myscala.math.stats.{ComputeStats, stats}
import myscala.output.SeqOfSeqExtensions.SeqOfSeqWriter
import myscala.output.SeqTuplesExtensions.SeqTuplesWriter
import trackingdataanalysis.visualization.Histogram
import visualization.PlotOptions

import scala.collection.JavaConverters._
import scala.collection.mutable.ArrayBuffer
import scala.collection.parallel.ForkJoinTaskSupport

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
  if ( config.getInt("sim.nb_runs") == 0 && !config.getBoolean("output.make_video")) {
    throw new IllegalArgumentException("No simulation to run ! Check parameters in config file.")
  }

  val simulationStartTime: Time = Time(config.getDouble("sim.start_time"))
  val simulationEndTime: Time = Time(config.getDouble("sim.end_time"))
  val n: Int = config.getInt("sim.nb_runs")
  val runSimulationsInParallel: Boolean = config.getBoolean("execution.parallel")
  val evaluationInterval: Time = Time(config.getDouble("sim.evaluate_dt"))


  // Runs the simulations in parallel or sequential based on the config file.
  val results: Vector[ResultsContainerNew] = {

    println("Preparing " + n + " simulations")

    if (config.getBoolean("output.make_video") && n == 0) {
      Vector(runSimulationWithVideo(config))
    }
    else if (config.getBoolean("output.make_video") && n > 0 && runSimulationsInParallel) {
      val simulationCollection: collection.parallel.ParSeq[SFGraphSimulator] = collection.parallel.ParSeq.fill(n - 1)(createSimulation(config))
      simulationCollection.tasksupport = new ForkJoinTaskSupport(new java.util.concurrent.ForkJoinPool(config.getInt("execution.threads")))
      simulationCollection.par.map(runAndCollect).seq.toVector :+ runSimulationWithVideo(config)
    }
    else if (config.getBoolean("output.make_video") && n > 0 && !runSimulationsInParallel) {
      val simulationCollection: collection.immutable.Vector[SFGraphSimulator] = collection.immutable.Vector.fill(n - 1)(createSimulation(config))
      simulationCollection.map(runAndCollect).seq.toVector :+ runSimulationWithVideo(config)
    }
    else if (!config.getBoolean("output.make_video") && n > 0 && runSimulationsInParallel) {
      val simulationCollection: collection.parallel.ParSeq[SFGraphSimulator] = collection.parallel.ParSeq.fill(n)(createSimulation(config))
      simulationCollection.tasksupport = new ForkJoinTaskSupport(new java.util.concurrent.ForkJoinPool(config.getInt("execution.threads")))
      simulationCollection.par.map(runAndCollect).seq.toVector
    }
    else {
      val simulationCollection: collection.immutable.Vector[SFGraphSimulator] = collection.immutable.Vector.fill(n)(createSimulation(config))
      simulationCollection.map(runAndCollect)
    }
  }

  //println(results.flatMap(_.completedPeds.map(_.travelTime.value)).stats)

  if (config.getBoolean("output.write_travel_times") || config.getBoolean("output.write_densities") ) {

    println("Processing results")

    // Collects then writes individual travel times to csv
    if (config.getBoolean("output.write_travel_times")) results
      .map(r => r.completedPeds.map(p => p.travelTime.value))
      .writeToCSV(
        config.getString("output.output_prefix") + "_travel_times.csv",
        columnNames = Some(Vector.fill(results.size)("r").zipWithIndex.map(t => t._1 + t._2.toString)),
        rowNames = None
      )

    // Collects then writes individual travel times with OD to csv
    if (config.getBoolean("output.write_travel_times")) results
      .zipWithIndex
      .flatMap(r => r._1.completedPeds.map(p => (r._2, p.travelTime, p.origin, p.finalDestination)))
      .writeToCSV(
        config.getString("output.output_prefix") + "_travel_times_OD.csv",
        columnNames = Some(Vector("run", "travel_time", "origin_id", "destination_id")),
        rowNames = None
      )

    // Collects times at which densities where measured
    val densityTimes: Vector[Time] = results.head.densityZones.head._2.densityHistory.unzip._1.toVector

     // writes densities to csv, first column is time, second column is mean, third column is var, then all individual densities
    if (config.getBoolean("output.write_densities") && config.getBoolean("sim.measure_density")) {
      for (i <- results.head.densityZones.keySet) {
        val densityStatsPerTime: Vector[(Int, Double, Double, Double, Double, Double)] = for (j <- densityTimes) yield {
          (for (k <- results.map(_.densityZones)) yield {
            k(i).densityHistory.find(_._1 == j).get._2
          }).stats
        }

        val densities: Vector[ArrayBuffer[Double]] = results.map(_.densityZones(i).densityHistory.map(_._2))
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

  // computes statistics on travel times and writes them
  if (config.getBoolean("output.write_tt_stats")) {
    results.map(r => stats(r.completedPeds.map(p => p.travelTime.value))).writeToCSV(config.getString("output.output_prefix") + "_travel_times_stats.csv", rowNames = None, columnNames = Some(Vector("size", "mean", "variance", "median", "min", "max")))
    new Histogram(config.getString("output.output_prefix") + "_travel_times_hist.png", results.flatMap(r => r.completedPeds.map(p => p.travelTime.value)), 1.0, "travel times [s]", "Histogram of travel times", PlotOptions(xmin=Some(20.0), xmax=Some(50.0), ymax=Some(0.3)))
  }

  if (config.getBoolean("output.write_trajectories_as_VS")) {
    println("Writing trajectories as VS to file")
    writePopulationTrajectories(results.head.completedPeds++results.head.uncompletedPeds, config.getString("output.output_prefix") + "_simulation_trajectories.csv")
  }


  // Analyse pedestrian data like travel time and walking speed by departure time interval.
  if (!config.getStringList("results-analysis.o_nodes").isEmpty && !config.getStringList("results-analysis.d_nodes").isEmpty )
  {
    val ODPairsToAnalyse: Iterable[(String, String)] = config.getStringList("results-analysis.o_nodes").asScala.zip(config.getStringList("results-analysis.d_nodes").asScala).map(t => (t._1, t._2))

    def findInterval(t: Double, times: Vector[Double]): Double = {
      times(times.indexWhere(_ > t))
    }

    def pedData: PedestrianSim => Double = ped => ped.travelDistance / ped.travelTime.value

    def pedWindows: PedestrianSim => Double = ped => findInterval(ped.entryTime.value, (simulationStartTime.value to simulationEndTime.value by evaluationInterval.value).toVector)

    def pedFilter: PedestrianSim => Boolean = ped => ODPairsToAnalyse.exists(_ == (ped.origin.name, ped.finalDestination.name))

    val speedByInteval: Vector[Vector[(Double, Double)]] = results.map(r => {
      val res = r.completedPeds.aggregateMetricByTimeWindow(pedFilter, pedData, pedWindows)
      (res.map( r => (r._1, r._2._2)).toVector ++ (simulationStartTime.value to simulationEndTime.value by evaluationInterval.value).filterNot(res.keySet.contains(_)).map(t => (t, Double.NaN))).sortBy(_._1)//
    })

    val speedStats: IndexedSeq[(Double, (Int, Double, Double, Double, Double, Double))] = (for (i <- simulationStartTime.value to simulationEndTime.value by evaluationInterval.value) yield {
      i -> (for (j <- results.indices if speedByInteval(j).exists(_._1 == i)) yield { speedByInteval(j).find(_._1 == i).get._2}).filterNot(_.isNaN).stats
    }).sortBy(_._1)
    (speedStats.map(_._1) +: speedStats.map(_._2._1) +: speedStats.map(_._2._2) +: speedStats.map(_._2._3) +:speedStats.map(_._2._4) +: speedByInteval.map(_.map(_._2))).writeToCSV(
      config.getString("output.output_prefix") + "-mean-speed-per-time-interval.csv",
      rowNames = None,
        columnNames = Some(Vector("time", "size", "mean", "variance", "median") ++ Vector.fill(results.size)("r").zipWithIndex.map(t => t._1 + t._2.toString))
      )

    def pedData2: PedestrianSim => Double = ped => ped.travelTime.value

    val ttByIntervals: IndexedSeq[IndexedSeq[(Double, Double)]] = results.map(r => {
      val res = r.completedPeds.aggregateMetricByTimeWindow(pedFilter, pedData2, pedWindows)
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

  }

  // ******************************************************************************************
  //                                  Processing for TRANS-FORM
  // ******************************************************************************************

  if (config.getBoolean("output.write_tt_4_transform")) {
    results.flatten(_.completedPeds).computeTT4TRANSFORM(0.0.to(100.0).by(config.getDouble("output.write_tt_4_transform_quantile_interval")), simulationStartTime, simulationEndTime, config.getString("output.write_tt_4_transform_file_name"))
  }
}

/*
,

 */