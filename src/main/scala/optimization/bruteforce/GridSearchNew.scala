package optimization.bruteforce

import java.io.File
import java.nio._
import java.nio.file.{Files, Path, Paths}

import myscala.math.stats.ComputeStats
import com.typesafe.config.Config
import hubmodel.DES.NOMADGraphSimulator
import hubmodel.output.image.{DrawControlDevicesAndWalls, DrawGraph, DrawWalls, DrawWallsAndGraph}
import hubmodel.{SimulatorParameters, createSimulation, runAndWriteResults}
import hubmodel.ped.PedestrianNOMAD
import hubmodel.supply.graph.{MultipleGraph, SingleGraph}
import hubmodel.{getDisaggPopulation, getFlows, getPTSchedule, insertDemandIntoSimulator}

import scala.collection.GenIterable

class ParameterModifications(i: Int)


abstract class GridSearchNew[T <: ParameterModifications](val config: Config) extends GridSearch {

  val defaultParameters: SimulatorParameters = createSimulation[PedestrianNOMAD](config).getSetupArguments

  // checks if the output dir exists for writing the results
  val outputDir: Path = Paths.get(config.getString("output.dir"))
  if (!Files.exists(outputDir) || !Files.isDirectory(outputDir)) {
    Files.createDirectory(outputDir)
  }


  val simulationRunsParameters: GenIterable[T]

  def getParameters(paramMods: T): SimulatorParameters

  def getRunPrefix(paramMods: T): String

  def runSimulations(): Unit = {
    for (p <- simulationRunsParameters) {
      val parameters: SimulatorParameters = getParameters(p)
      val sim = new NOMADGraphSimulator[PedestrianNOMAD](
        parameters._1,
        parameters._2,
        parameters._3,
        parameters._4,
        parameters._5,
        parameters._6,
        parameters._7,
        parameters._8,
        parameters._9,
        parameters._10,
        parameters._11,
        config.getBoolean("output.write_trajectories_as_VS") || config.getBoolean("output.write_trajectories_as_JSON")
      )

      ////////////////////////////////////////////////////////////////////////////////////////////////

      // Creates images representing the walls, route graph and both overlaid.
      val wallsImage = new DrawWalls(sim.walls, config.getString("output.output_prefix") + "_wallsWithNames.png", showNames = true)
      sim.graph match {
        case rm: MultipleGraph => { rm.getGraphs.foreach(g =>  new DrawGraph(g._2._2.edgeCollection.map(e => (e.startVertex, e.endVertex)).toVector, config.getString("output.output_prefix") + "_graph_" + g._1 + ".png"))}
        case rs: SingleGraph => { new DrawGraph(sim.graph.edges.map(e => (e.startVertex, e.endVertex)).toVector, config.getString("output.output_prefix") + "_graph.png") }
      }
      val fullImage = new DrawWallsAndGraph(sim.walls, sim.graph.edges.map(e => (e.startVertex, e.endVertex)).toVector, config.getString("output.output_prefix") + "_wallsAndGraph.png")
      val devicesImage = new DrawControlDevicesAndWalls(config.getString("output.output_prefix") + "_wallsWithDevices.png", sim.walls, sim.controlDevices)


      ///////////////////////////////////////////////////////////////////////////////////////////////

      val flows = getFlows(config)

      val (timeTable, stop2Vertex) = getPTSchedule(flows, config)

      val disaggPopulation = getDisaggPopulation(config)

      insertDemandIntoSimulator[PedestrianNOMAD](sim, disaggPopulation, flows, timeTable)

      runAndWriteResults(
        sim,
        getRunPrefix(p),
        if (!config.getIsNull("output.dir")) {
          Some(config.getString("output.dir"))
        } else {
          None
        },
        config.getBoolean("output.write_trajectories_as_VS"),
        config.getBoolean("output.write_trajectories_as_JSON")
      )
      System.gc()
    }
  }

  def groupResultsFiles: Map[String, List[File]] = { // reads the files and process the data
    new File(outputDir.getFileName.toString).listFiles.filter(_.isFile).toList.groupBy(f => {
      f.getName match {
        case a if a.contains("_params_individual_density_") => "individual_density"
        case a if a.contains("_params_tt_") => "tt"
        case a if a.contains("_params_density_") => "density"
      }
    })
  }

  def processTTResults(files: List[File], numberParameters: Int): Map[(Double, Double), (Int, Double, Double, Double, Double, Double)] = {

    files.map(ProcessTTFile2Parameters).
      flatMap(tup => tup._3.map(t => (tup._1, tup._2, t._1._1, t._1._2, t._2))).
      groupBy(tup => (tup._1, tup._2)).
      mapValues(v => v.flatMap(_._5).stats)
  }


}
