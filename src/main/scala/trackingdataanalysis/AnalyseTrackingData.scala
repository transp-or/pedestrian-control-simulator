package trackingdataanalysis

/**
  * Created by nicholas on 2/15/17.
  */

import com.typesafe.config.Config
import hubmodel.parseConfigFile
import kn.uni.voronoitreemap.j2d.PolygonSimple
import myscala.math.stats.ComputeStats
import myscala.output.SeqTuplesExtensions.SeqTuplesWriter
import trackingdataanalysis.pedtrack.io.{CONTROLLED, MonitoredAreaReader, UNCONTROLLED}
import trackingdataanalysis.pedtrack.visiosafe.{MultiDayAggregateProcessor, SingleDayAggregateProcessor}
import trackingdataanalysis.pedtrack.{ZoneProcessing, Pedestrian, norm}
import trackingdataanalysis.visualization.{Histogram, PlotOptions, ScatterPlot}

import scala.collection.JavaConverters._

object AnalyseTrackingData extends App {


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

  val config: Config = parseConfigFile(args)

  // default config
  /* val defaultConfig = ConfigFactory.parseResources("reference.conf")

   // Reads the file passed as argument
   val config = ConfigFactory.load(confFile).withFallback(defaultConfig)

   // checkValid(), just as in the plain SimpleLibContext.
   // Note that these fields are NOT lazy, because if we're going to
   // get any exceptions, we want to get them on startup.
   config.checkValid(ConfigFactory.defaultReference())*/

  // ******************************************************************************************
  //                    Extract recurrent parameters to variables
  // ******************************************************************************************

  val startTime: Double = config.getDouble("parameters.start_time")
  val endTime: Double = config.getDouble("parameters.end_time")
  val dt: Double = config.getDouble("parameters.dt")

  println("Time interval is: " + dt + " seconds")

  // time intevals used to copute time average data
  val times = BigDecimal(startTime) to endTime by dt

  // intermediate times used to compute fixed-time data (snapshots)
  val timesMiddles = BigDecimal(startTime + 0.5 * dt) to endTime by dt

  // tracking data to process
  val pathToData: String = config.getString("files.data-dir")
  val fileNames: Vector[String] = config.getStringList("files.tracking").asScala.toVector
  val files: Vector[String] = config.getStringList("files.tracking").asScala.map(pathToData + _).toVector
  val entryExitZones: String = config.getString("files.entry-exit-zones")


  // ******************************************************************************************
  //                                  Process data
  // ******************************************************************************************
  val processedData: ZoneProcessing = if (files.size == 1) {
    new SingleDayAggregateProcessor(
      files.head,
      entryExitZones,
      0.05)
  } else if (files.size > 1) {
    new MultiDayAggregateProcessor(
      files,
      entryExitZones,
      0.05
    )
  } else {
    throw new IllegalArgumentException("No files are listed for processing")
  }


  processedData match {
    case data: SingleDayAggregateProcessor => {

      // Reads ares to compute indicatros inside
      val monitoredZone = new MonitoredAreaReader(config.getString("files.monitored-zones"))
      val mainZone = monitoredZone.monitoredArea
      println("Main zone area is " + mainZone.area + "[m^2]")

      val ODPairsToAnalyse: Iterable[(Int, Int)] = config.getStringList("results-analysis.o_nodes").asScala.zip(config.getStringList("results-analysis.d_nodes").asScala).map(t => (t._1.toInt, t._2.toInt))


      //data.writePedestriansToJSON("test.json")

      def flattenTuple[T: Numeric](t: (Int, Int, (Int, Double, Double, Double, T, T))): (Int, Int, Int, Double, Double, Double, T, T) = (t._1, t._2, t._3._1, t._3._2, t._3._3, t._3._4, t._3._5, t._3._6)

      (for (od <- ODPairsToAnalyse.toVector) yield {
        flattenTuple(od._1, od._2, data.ped.values.filter(p => p.oZone == od._1 && p.dZone == od._2).map(_.travelTime).stats)
      }).writeToCSV(fileNames.head + "_travel_times_OD_stats.csv", columnNames = Some(Vector("O", "D", "size", "mean", "variance", "median", "min", "max")), rowNames = None)

      val flowsAgain = data.computeFlowsAgain(monitoredZone.flowLines, times.toVector.map(_.toDouble))

      val lengthControlledInflow: Double = monitoredZone.flowLines.filter(_.inflowType == CONTROLLED).map(fl => norm(fl.line._1, fl.line._2)).sum
      //7.37//1.0/2.0
      val lengthTotalFlow: Double = monitoredZone.flowLines.map(fl => norm(fl.line._1, fl.line._2)).sum
      val lengthUncontrolledInflow: Double = monitoredZone.flowLines.filter(_.inflowType == UNCONTROLLED).map(fl => norm(fl.line._1, fl.line._2)).sum //12.16//6.800 + 6.800

      val inflowControlled: Array[Double] = flowsAgain._1.toVector.sortBy(_._1).map(_._2 / (dt * lengthControlledInflow)).toArray
      //flowArray.map(_(2))
      val inflowUncontrolled = flowsAgain._2.toVector.sortBy(_._1).map(_._2 / (dt * lengthUncontrolledInflow)).toArray
      //flowArray.map(_(1))
      val outflow = flowsAgain._3.toVector.sortBy(_._1).map(_._2 / (dt * lengthTotalFlow)).toArray //flowArray.map(_(0))

      val edieComponents = data.computeEdieComponents(mainZone, times.toVector.map(_.toDouble))

      val edieDensity: Vector[Double] = edieComponents.map(r => r._2._1 / (mainZone.area * dt))
      val edieSpeed: Vector[Double] = edieComponents.map(d => scala.math.sqrt(scala.math.pow(d._2._2 / d._2._1, 2) + scala.math.pow(d._2._3 / d._2._1, 2))) // edieComponents.map(d => d._2._3/d._2._1)
      val edieFlow: Vector[Double] = edieDensity.zip(edieSpeed).map(d => d._1 * d._2) //edieComponents.map(d => d._2._3) //edieComponents.map(d => scala.math.sqrt(scala.math.pow(d._2._2/(mainZone.area * dt),2) + scala.math.pow(d._2._3/(mainZone.area * dt),2)))


      val edieResults = (edieDensity, edieFlow, edieSpeed).zipped.toVector

      val box: PolygonSimple = new PolygonSimple
      box.add(mainZone.corners(0).X, mainZone.corners(0).Y)
      box.add(mainZone.corners(1).X, mainZone.corners(1).Y)
      box.add(mainZone.corners(2).X, mainZone.corners(2).Y)
      box.add(mainZone.corners(3).X, mainZone.corners(3).Y)


      val voronoiDensities = data.computeVoronoiDensity(mainZone, timesMiddles.map(_.toDouble), box)


      val accumulationDensities = data.computeAccumulationDensity(mainZone, timesMiddles.map(_.toDouble))
      //println(mainZone)
      //println(accumulationDensities)

      /*val fitResults: (Double, Double, Double) = data.computeControllerParams(
        new DenseVector(inflowControlled),
        new DenseVector(inflowUncontrolled),
        new DenseVector(outflow),
        new DenseVector(voronoiDensities.map(_._2).toArray),
        new DenseVector(edieFlow.toArray),
        1.0, // target density pax/m^2
        0.25 // target inflow pax/m/s
      )

      println(fitResults)*/


      // computing metric with starting times in intervals
      def pedData: Pedestrian => Double = ped => ped.meanVelocity

      def pedWindows: Pedestrian => Int = ped => data.findInterval(ped.entryTime, (BigDecimal(startTime) to endTime by dt).toVector.map(_.toDouble))

      def meanIterable: Iterable[Double] => (Int, Double, Double, Double, Double, Double) = iterable => iterable.stats

      def pedFilter: Pedestrian => Boolean = ped => ODPairsToAnalyse.exists(_ == (ped.oZone, ped.dZone))

      val speedByEntyTimeMean: Map[Int, (Int, Double, Double, Double, Double, Double)] = data.aggregateMetricByTimeWindow(pedFilter, pedData, pedWindows, meanIterable)

      data.ped.values.filter(pedFilter).groupBy(p => (data.findInterval(p.entryTime, (BigDecimal(startTime) to endTime by 60.0).toVector.map(_.toDouble)), p.oZone, p.dZone)).map(r => r._1 -> r._2.map(_.travelTime)).toVector.sorted.writeToCSV("test.csv")
      speedByEntyTimeMean.toVector.sortBy(_._1).writeToCSV(config.getString("output.prefix") + "-TTByEntyTimeMean.csv")
      println("Stats of the speed per time interval:\n" + speedByEntyTimeMean.values.map(_._2).stats)

      new Histogram(config.getString("output.prefix") + "-meanSpeedPerDepartureTimeHistogram.png", speedByEntyTimeMean.map(_._2._2), 0.05, "speed [m/s]", "Mean speed per departure time", PlotOptions(xmin = Some(0.5), xmax = Some(1.5)))

      val opts = PlotOptions(xTickInterval = 0.5, yTickInterval = 0.25, xmin = Some(0.0), xmax = Some(6), width = 800, height = 600, ymin = Some(0.0), ymax = Some(3))

      new ScatterPlot(config.getString("output.prefix") + "-edieVelocity-VS-edieFlow-" + dt.toInt + "sec.png", edieDensity, edieSpeed, "edie density [pax/m^2]", "edie velocity [m/s]", opts = opts)
      new ScatterPlot(config.getString("output.prefix") + "-edieDensity-VS-edieFlow-" + dt.toInt + "sec.png", edieDensity, edieFlow, "edie density [pax/m^2]", "generalized flow [pax/m/s]", opts = opts)
      new ScatterPlot(config.getString("output.prefix") + "-voronoiDensity-VS-edieFlow-" + dt.toInt + "sec.png", voronoiDensities.map(_._2), edieFlow, "voronoi density [pax/m^2]", "generalized flow [pax/m/s]", opts = opts)
      new ScatterPlot(config.getString("output.prefix") + "-accumulationDensity-VS-edieFlow-" + dt.toInt + "sec.png", accumulationDensities.map(_._2).toVector, edieFlow, "accumulation density [pax/m^2]", "generalized flow [pax/m/s]", opts = opts)
      //new RegressionPlot(config.getString("output.prefix") + "-edieFlow-VS-outflow-reg" + dt.toInt + "sec.png", edieFlow , outflow.toVector, "generalized flow [pax/m/s]", "outflow [pax/m/s]", fitResults._1, opts = PlotOptions(xTickInterval = 0.1 ,yTickInterval = 0.1, xmin=Some(0.0), xmax=Some(1.15), width = 800, height = 600, ymin = Some(0.0), ymax = Some(0.4)))


      /*
            new MovingPedestriansWithVoronoiDensity(config.getString("output.prefix") + "_testVidmovingVoronoi.mp4",
              Option("/home/nicholas/visiosafe-data/lausanne-metadata/zoneid-lausanne-piw.png"), // None
              (90.75, 24.75),
              4,
              data.getVoronoiData(mainZone, timesMiddles, box).map(d => (d._1, d._2.map(_._3))),
              data.ped.toMap,
              data.timeMapFuncVal(timesMiddles),
              timesMiddles)
      */

    }
    case data: MultiDayAggregateProcessor => {

      data.writePedestriansToJSON("disaggregate-demand.json")

      /*def pedData: Pedestrian => Double = ped => ped.travelTime
      def pedWindows: Pedestrian => Int = ped => data.findInterval(ped.entryTime, (startTime to endTime by 60.0).toVector)
      def meanIterable: Iterable[Double] => (Int, Double, Double, Double, Double, Double) = iterable => iterable.stats
      def pedFilter: Pedestrian => Boolean = ped => ODPairsToAnalyse.exists(_ == (ped.oZone, ped.dZone))
      val TTByEntyTimeMean: Map[DataSpecification, Map[Int, (Int, Double, Double, Double, Double, Double)]] = data.aggregateMetricByTimeWindow(pedFilter, pedData, pedWindows, meanIterable)*/

      //val opts = PlotOptions(xTickInterval = 0.2 ,yTickInterval = 0.1, xmin=Some(0.0), xmax=Some(3.0), width = 800, height = 600, ymin = Some(0.0), ymax = Some(2.0))
      /*
            new ScatterPlot(config.getString("output.prefix") + "-edieVelocity-VS-edieFlow-" + dt.toInt + "sec.png", edieDensity, edieSpeed,  "edie density [pax/m^2]", "edie velocity [m/s]", opts=opts)
            new ScatterPlot(config.getString("output.prefix") + "-edieDensity-VS-edieFlow-" + dt.toInt + "sec.png", edieDensity , edieFlow, "edie density [pax/m^2]", "generalized flow [pax/m/s]", opts=opts)
            new ScatterPlot(config.getString("output.prefix") + "-voronoiDensity-VS-edieFlow-" + dt.toInt + "sec.png", voronoiDensities.map(_._2) , edieFlow, "voronoi density [pax/m^2]", "generalized flow [pax/m/s]", opts=opts)
            new ScatterPlot(config.getString("output.prefix") + "-accumulationDensity-VS-edieFlow-" + dt.toInt + "sec.png", accumulationDensities.map(_._2).toVector , edieFlow, "accumulation density [pax/m^2]", "generalized flow [pax/m/s]", opts=opts)
            new RegressionPlot(config.getString("output.prefix") + "-edieFlow-VS-outflow-reg" + dt.toInt + "sec.png", edieFlow , outflow.toVector, "generalized flow [pax/m/s]", "outflow [pax/m/s]", fitResults._1, opts = opts)
      */
      //TTByEntyTimeMean.toVector.sortBy(_._1).writeToCSV(config.getString("output.prefix") + "-TTByEntyTimeMean.csv")
      //println("Stats of the mean TT per time interval:")
      //TTByEntyTimeMean.foreach(dayPop => println(dayPop._1 + dayPop._2.map(_._2._2).stats.toString))
    }
  }
}
