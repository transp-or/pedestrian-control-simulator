import RunSimulation.args
import com.typesafe.config.{Config, ConfigFactory}
import optimization.bruteforce.ParameterExploration
import hubmodel.createSimulation
import myscala.output.SeqTuplesExtensions.SeqTuplesWriter
import trackingdataanalysis.visualization.HeatMap
import visualization.PlotOptions

object ExploreParameters extends App {

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

  val parameterGridSearch: ParameterExploration = new ParameterExploration(createSimulation(config), config)

  val results = parameterGridSearch.exploreFlowGateFunctionalFormLinear((0.25, 4.25, 16), (0.0, 4.0, 15))


  results.map(r => (r._1._1, r._1._2, r._2._1._1, r._2._1._2, r._2._1._3, r._2._1._4, r._2._1._5, r._2._1._6)).toVector.writeToCSV(config.getString("output.output_prefix") + "_exploration-results-travel-time.csv")
  results.map(r => (r._1._1, r._1._2, r._2._2._1, r._2._2._2, r._2._2._3, r._2._2._4, r._2._2._5, r._2._2._6)).toVector.writeToCSV(config.getString("output.output_prefix") + "_exploration-results-density.csv")


  new HeatMap(config.getString("output.output_prefix") + "_heatmap-mean-tt.png", results.map(r => (r._1._1, r._1._2, r._2._1._2)), "mean travel time", "constant", "linear")
  new HeatMap(config.getString("output.output_prefix") + "_heatmap-variance-tt.png", results.map(r => (r._1._1, r._1._2, r._2._1._3)), "var travel time", "constant", "linear")
  new HeatMap(config.getString("output.output_prefix") + "_heatmap-median-tt.png", results.map(r => (r._1._1, r._1._2, r._2._1._4)), "median travel time", "constant", "linear")


  new HeatMap(config.getString("output.output_prefix") + "_heatmap-mean-density.png", results.map(r => (r._1._1, r._1._2, r._2._2._2)), "mean density", "constant", "linear")
  new HeatMap(config.getString("output.output_prefix") + "_heatmap-variance-density.png", results.map(r => (r._1._2, r._1._2, r._2._2._3)), "var density", "constant", "linear")
  new HeatMap(config.getString("output.output_prefix") + "_heatmap-median-density.png", results.map(r => (r._1._2, r._1._2, r._2._2._4)), "median density", "constant", "linear")

}