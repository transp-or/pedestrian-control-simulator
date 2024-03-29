package hubmodel.results


import com.typesafe.config.{Config, ConfigFactory}
import myscala.math.stats.ComputeQuantiles
import org.apache.commons.math3.stat.inference.KolmogorovSmirnovTest


object CompareSimulationResults extends App {


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


  // Reads results from both dirs
  val refResults: Vector[ResultsContainerRead] = readResults(config.getString("dirs.reference-dir"), config.getString("output.output_prefix")).toVector
  val otherResults: Vector[ResultsContainerRead] = readResults(config.getString("dirs.other-dir"), config.getString("output.output_prefix")).toVector


  // Processing results
  println("Processing results")

  val KS = new KolmogorovSmirnovTest()

  // Computes statistics about densites
  if (config.getBoolean("output.density-stats")) {
    val refAggDensities: Iterable[Double] = refResults.flatMap(_.monitoredAreaDensity.get._2.flatten).filter(_ > 0).take(100)
    val otherAggDensities: Iterable[Double] = otherResults.flatMap(_.monitoredAreaDensity.get._2.flatten).filter(_ > 0).take(100)

    println(KS.kolmogorovSmirnovTest(refAggDensities.toArray, otherAggDensities.toArray))
    //println(KS.kolmogorovSmirnovStatistic(refAggDensities.toArray, otherAggDensities.toArray))

  }

  if (config.getBoolean("output.tt-stats")) {

    val refAggDensities: Iterable[Double] = refResults.flatMap(_.tt.map(_._3)).cutOfAfterQuantile(99.9).take(1000)
    val otherAggDensities: Iterable[Double] = otherResults.flatMap(_.tt.map(_._3)).cutOfAfterQuantile(99.9).take(1000)

    println(KS.kolmogorovSmirnovTest(refAggDensities.toArray, otherAggDensities.toArray))
    //println(KS.kolmogorovSmirnovStatistic(refAggDensities.toArray, otherAggDensities.toArray))

  }


}
