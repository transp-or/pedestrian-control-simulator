import java.io.File

import com.typesafe.config.{Config, ConfigFactory}
import hubmodel.io.output.image.{DrawGraph, DrawWalls, DrawWallsAndGraph}
import hubmodel.ped.PedestrianNOMAD
import hubmodel.supply.continuous.ReadContinuousSpace
import hubmodel.supply.graph.readGraph
import hubmodel.io.output.tikz.writeEdgesAsTikz


object makePictures extends App {

  case class CLIConfig(walls: File = new File("."), graph: File = new File("."), prefix: String = "", configFile: File = new File("."), showWallNames: Boolean = false)

  val parser = new scopt.OptionParser[CLIConfig]("scopt") {
    head("scopt", "3.x")

    opt[File]('w', "walls").optional().valueName("<file>")
      .action((x, c) => c.copy(walls = x))
      .text("required, JSON specification of all the walls building the infrastructure")

    opt[File]('g', "graph").optional().valueName("<file>")
      .action((x, c) => c.copy(graph = x))
      .text("required, JSON specification of the route choice graph")

    opt[String]('p', "prefix").optional().valueName("String")
      .action((x, c) => c.copy(prefix = x))
      .text("prefix to append in front of the output file names")

    opt[Unit]('s', "showWallNames").action((_, c) =>
      c.copy(showWallNames = true)).text("showWallNames is a flag: should the names of the walls be printed")

    opt[File]('c', "conf").required().valueName("<file>")
        .action((x,c) => c.copy(configFile = x))
        .text("configuration file used for simulations")

    //opt[Unit]("verbose").action( (_, c) =>
    //  c.copy(verbose = true) ).text("verbose is a flag")

    //opt[Unit]("debug").hidden().action( (_, c) =>
    //  c.copy(debug = true) ).text("this option is hidden in the usage text")

    help("help").text("prints this usage text")

  }


  parser.parse(args, CLIConfig()) match {

    case Some(config) =>

      val tmpConfig: Config = ConfigFactory.load(ConfigFactory.parseFile(config.configFile))

      val infraGraph = readGraph(tmpConfig.getString("files.graph"), tmpConfig.getBoolean("sim.use_flow_gates"), tmpConfig.getBoolean("sim.use_binary_gates"), tmpConfig.getBoolean("sim.use_amw"), tmpConfig.getBoolean("sim.use_flow_sep"), tmpConfig.getBoolean("sim.fixed_flow_sep"), tmpConfig.getBoolean("sim.measure_density"), tmpConfig.getBoolean("sim.use_alternate_graphs"), (tmpConfig.getString("sim.amws_mode"),tmpConfig.getString("sim.amws_reactive_mode")), (tmpConfig.getDouble("sim.route_choice_TD_beta"), tmpConfig.getDouble("sim.route_choice_PSC_beta")))

      val parserCont = new ReadContinuousSpace(tmpConfig.getString("files.walls"))
      val infraSF = parserCont.continuousSpace

      val prefixToAdd: String = {
        if (config.prefix.isEmpty) ""
        else config.prefix + '_'
      }

      val showNamesOnFigures: Boolean = if (config.showWallNames) {
        true
      } else {
        false
      }

      val wallsImage = new DrawWalls(infraSF.walls, prefixToAdd + "wallImage.png", showNames = showNamesOnFigures)
      val graphImage = new DrawGraph(infraGraph._1.edges.map(e => (e.startVertex, e.endVertex)).toVector, prefixToAdd + "graphImage.png", showNames = showNamesOnFigures)
      val fullImage = new DrawWallsAndGraph(infraSF.walls, infraGraph._1.edges.map(e => (e.startVertex, e.endVertex)).toVector, prefixToAdd + "wallAndGraphImage.png", showNames = showNamesOnFigures)

      writeEdgesAsTikz(prefixToAdd + "edges.tex", infraGraph._1.edges, infraGraph._1.vertexMapNew.values, infraSF.walls)

    case None => println("Probably an error when passing the parameters, or an unknown parameter was given.")

  }

}