import com.typesafe.config.{Config, ConfigFactory}
import optimization.bruteforce.FlowSensitivity

object ReadExploreSensitivityFlowsResults extends App {

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
  println(confFile)
  val config: Config = ConfigFactory.load(confFile)

  val flowSens: FlowSensitivity = new FlowSensitivity(config)

  val results = flowSens.processWrittenResults
  flowSens.drawResults(results)

  val resultsSplitOD = flowSens.processWrittenResultsSplitOD
  flowSens.drawResultsSplitOD(resultsSplitOD)


  if (!config.getIsNull("output.compare_results_to")) {
    println(config.getString("output.compare_results_to"))
    flowSens.drawComparisonResults("simulation/flow-sensitivity-fixed-sep.conf")
  }


}