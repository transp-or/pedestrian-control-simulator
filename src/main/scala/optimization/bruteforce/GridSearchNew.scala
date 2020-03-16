package optimization.bruteforce

import java.io.File
import java.nio.file.{Files, Path, Paths}

import com.typesafe.config.Config
import hubmodel.DES.{NOMADGraphSimulator, _}
import hubmodel._
import hubmodel.demand.{AggregateFlows, PedestrianFlowFunction_New, PedestrianFlowPT_New, PedestrianFlow_New}
import hubmodel.ped.PedestrianNOMAD
import myscala.math.stats.ComputeStats

import scala.collection.GenIterable


abstract class GridSearchNew[T <: ParameterModifications](val config: Config) extends GridSearch {

  val defaultParameters: SimulationInputParameters = createSimulation(config).getSetupArgumentsNew

  // checks if the output dir exists for writing the results
  val outputDir: Path = Paths.get(config.getString("output.dir"))
  if (!Files.exists(outputDir) || !Files.isDirectory(outputDir)) {
    Files.createDirectory(outputDir)
  }

  val simulationRunsParameters: IterableOnce[T]

  def getParameters(paramMods: T): SimulationInputParameters

  def getRunPrefix(paramMods: T): String

  def getFlowMods(paramMods: T): (Iterable[PedestrianFlow_New], Iterable[PedestrianFlowPT_New], Iterable[PedestrianFlowFunction_New])

  def runSimulations(): Unit = {
    for (p <- simulationRunsParameters) {
      val parameters: SimulationInputParameters = getParameters(p)
      parameters.logFullPedestrianHistory = config.getBoolean("output.write_trajectories_as_VS") || config.getBoolean("output.write_trajectories_as_JSON")
      val sim = new PedestrianSimulation(parameters)

      ////////////////////////////////////////////////////////////////////////////////////////////////

      // Creates images representing the walls, route graph and both overlaid.
      /*val wallsImage = new DrawWalls(sim.walls, config.getString("output.output_prefix") + "_wallsWithNames.png", showNames = true)
      sim.graph match {
        case rm: MultipleGraph => { rm.getGraphs.foreach(g =>  new DrawGraph(g._2._2.edgeCollection.map(e => (e.startVertex, e.endVertex)).toVector, config.getString("output.output_prefix") + "_graph_" + g._1 + ".png"))}
        case rs: SingleGraph => { new DrawGraph(sim.graph.edges.map(e => (e.startVertex, e.endVertex)).toVector, config.getString("output.output_prefix") + "_graph.png") }
      }
      val fullImage = new DrawWallsAndGraph(sim.walls, sim.graph.edges.map(e => (e.startVertex, e.endVertex)).toVector, config.getString("output.output_prefix") + "_wallsAndGraph.png")
      val devicesImage = new DrawControlDevicesAndWalls(config.getString("output.output_prefix") + "_wallsWithDevices.png", sim.walls, sim.controlDevices)
*/

      ///////////////////////////////////////////////////////////////////////////////////////////////

      val flows: AggregateFlows = getFlowMods(p)

      val (timeTable, stop2Vertex) = getPTSchedule(config)

      val disaggPopulation = getDisaggregateFlows(config)

      insertDemandIntoSimulator(sim, disaggPopulation, flows, timeTable)

      runAndWriteResults(
        sim,
        getRunPrefix(p),
        config.getString("output.dir"),
        config.getBoolean("output.write_trajectories_as_VS"),
        config.getBoolean("output.write_trajectories_as_JSON")
      )
      System.gc()
    }
  }

  def groupResultsFiles: Map[String, List[File]] = { // reads the files and process the data
    new File(outputDir.getParent.toString + "/" + outputDir.getFileName.toString).listFiles.filter(_.isFile).toList.groupBy(f => {
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
      groupBy(tup => (tup._1, tup._2)).view.
      mapValues(v => v.flatMap(_._5).stats).toMap
  }


}
