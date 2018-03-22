import java.io.File

import hubmodel.output.image.{DrawGraph, DrawWalls, DrawWallsAndGraph}
import hubmodel.supply.continuous.ContinuousSpaceReader
import hubmodel.supply.graph.GraphReader


object makePictures extends App {

  case class Config(walls: File = new File("."), graph: File = new File("."), prefix: String = "", showWallNames: Boolean = false)

  val parser = new scopt.OptionParser[Config]("scopt") {
    head("scopt", "3.x")

    opt[File]('w', "walls").required().valueName("<file>")
      .action((x, c) => c.copy(walls = x))
      .text("required, JSON specification of all the walls building the infrastructure")

    opt[File]('g', "graph").required().valueName("<file>")
      .action((x, c) => c.copy(graph = x))
      .text("required, JSON specification of the route choice graph")

    opt[String]('p', "prefix").optional().valueName("String")
      .action((x, c) => c.copy(prefix = x))
      .text("prefix to append in front of the output file names")

    opt[Unit]('s', "showWallNames").action((_, c) =>
      c.copy(showWallNames = true)).text("showWallNames is a flag: should the names of the walls be printed")


    //opt[Unit]("verbose").action( (_, c) =>
    //  c.copy(verbose = true) ).text("verbose is a flag")

    //opt[Unit]("debug").hidden().action( (_, c) =>
    //  c.copy(debug = true) ).text("this option is hidden in the usage text")

    help("help").text("prints this usage text")

  }

  parser.parse(args, Config()) match {

    case Some(config) =>
      val infraGraph = new GraphReader(config.graph.toString, false, false, false, false, false)
      val parserCont = new ContinuousSpaceReader(config.walls.toString)
      val infraSF = parserCont.continuousSpace

      val prefixToAdd: String = {
        if (config.prefix.isEmpty) ""
        else config.prefix + '_'
      }

      val wallsImage = new DrawWalls(infraSF.walls, prefixToAdd + "wallImage.png", showNames = config.showWallNames)
      val graphImage = new DrawGraph(infraGraph.graph.standardEdges.map(e => (e.startVertex, e.endVertex)).toVector, prefixToAdd + "graphImage.png")
      val fullImage = new DrawWallsAndGraph(infraSF.walls, infraGraph.graph.standardEdges.map(e => (e.startVertex, e.endVertex)).toVector, prefixToAdd + "wallAndGraphImage.png")

    case None => println("Probably an error when passing the parameters, or an unknown parameter was given.")
  }

}