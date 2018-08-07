import ExploreParameters.{args, config}
import com.typesafe.config.{Config, ConfigFactory}
import hubmodel.createSimulation
import optimization.bruteforce.ParameterExploration
import trackingdataanalysis.visualization.HeatMap
import visualization.PlotOptions
import myscala.output.SeqTuplesExtensions.SeqTuplesWriter

object ReadExploreParametersResults extends App {

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

  val results = new ParameterExploration(config).processWrittenResults

  results.map(r => (r._1._1, r._1._2, r._2._1._1, r._2._1._2, r._2._1._3, r._2._1._4, r._2._1._5, r._2._1._6)).toVector.writeToCSV(config.getString("output.output_prefix") + "_exploration-results-travel-time.csv")
    results.map(r => (r._1._1, r._1._2, r._2._2._1, r._2._2._2, r._2._2._3, r._2._2._4, r._2._2._5, r._2._2._6)).toVector.writeToCSV(config.getString("output.output_prefix") + "_exploration-results-density.csv")
    

 {
    val fileTT: String = config.getString("output.output_prefix") + "_exploration-results-travel-time.csv"

    val bufferedSource = io.Source.fromFile("/home/molyneau/code/hub-simulator/" + fileTT)
    val resultsRead: Iterable[Array[Double]] = (for (line <- bufferedSource.getLines) yield {
//      println(line.split(",").map(_.trim))
      line.split(",").map(_.trim).map(_.toDouble)
    }).toVector
    bufferedSource.close

    resultsRead.foreach(r => println(r(0), r(1), r(2)))

    new HeatMap(fileTT + "_heatmap-mean-tt.png", resultsRead.map(r => (r(0), r(1), r(3))), "mean travel time", "constant", "linear", PlotOptions(zmin=Some(25), zmax=Some(30)))
    new HeatMap(fileTT + "_heatmap-variance-tt.png", resultsRead.map(r => (r(0), r(1), r(4))), "var travel time", "constant", "linear", PlotOptions(zmin=Some(5), zmax=Some(30)))
    new HeatMap(fileTT + "_heatmap-median-tt.png", resultsRead.map(r => (r(0), r(1), r(5))), "median travel time", "constant", "linear", PlotOptions(zmin=Some(25), zmax=Some(30)))
  }

  {
    val fileDensity: String = config.getString("output.output_prefix") + "_exploration-results-density.csv"
    
    val bufferedSource = io.Source.fromFile("/home/molyneau/code/hub-simulator/" + fileDensity)
    val resultsRead: Iterable[Array[Double]] = (for (line <- bufferedSource.getLines) yield {
  //    println(line.split(",").map(_.trim))
      line.split(",").map(_.trim).map(_.toDouble)
    }).toVector
    bufferedSource.close

    new HeatMap(fileDensity + "_heatmap-mean-density.png", resultsRead.map(r => (r(0), r(1), r(3))), "mean density", "constant", "linear", PlotOptions(zmin=Some(0.5), zmax=Some(2.5)))
    new HeatMap(fileDensity + "_heatmap-variance-density.png", resultsRead.map(r => (r(0), r(1), r(4))), "var density", "constant", "linear")
    new HeatMap(fileDensity + "_heatmap-median-density.png", resultsRead.map(r => (r(0), r(1), r(5))), "median density", "constant", "linear", PlotOptions(zmin=Some(0.5), zmax=Some(2.5)))
  }
  
}