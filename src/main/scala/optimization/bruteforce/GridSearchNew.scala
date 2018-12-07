package optimization.bruteforce

import java.io.File
import java.nio._
import java.nio.file.{Files, Path, Paths}

import myscala.math.stats.ComputeStats

import com.typesafe.config.Config
import hubmodel.DES.NOMADGraphSimulator
import hubmodel.{SimulatorParameters, createSimulation, runAndWriteResults}
import hubmodel.ped.PedestrianNOMAD

import hubmodel.{getDisaggPopulation, getFlows, insertDemandIntoSimulator, getPTSchedule}

import scala.collection.GenIterable

class ParameterModifications(i: Int)


abstract class GridSearchNew[T <: ParameterModifications](val config: Config) extends GridSearch {

  println(config)

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
        parameters._11
      )

      val flows = getFlows(config)

      val (timeTable, stop2Vertex) = getPTSchedule(flows, config)

      val disaggPopulation = getDisaggPopulation(config)

      insertDemandIntoSimulator[PedestrianNOMAD](sim, disaggPopulation, flows, timeTable)

      runAndWriteResults(sim, getRunPrefix(p), if (!config.getIsNull("output.dir")) {
        Some(config.getString("output.dir"))
      } else {
        None
      })
      System.gc()
    }
  }

  def groupResultsFiles: Map[String, List[File]] = { // reads the files and process the data
    println(outputDir.getFileName.toString)
    new File(outputDir.getFileName.toString).listFiles.filter(_.isFile).toList.groupBy(f => {
      f.getName match {
        case a if a.contains("_params_individual_density_") => "individual_density"
        case a if a.contains("_params_tt_") => "tt"
        case a if a.contains("_params_density_") => "density"
      }
    })
  }

  def processTTResults(files: List[File], numberParameters: Int) : Map[(Double, Double), (Int, Double, Double, Double, Double, Double)] = {

      files.map(ProcessTTFile2Parameters).
      flatMap(tup => tup._3.map(t => (tup._1, tup._2, t._1._1, t._1._2, t._2))).
      groupBy(tup => (tup._1, tup._2)).
      mapValues(v => v.flatMap(_._5).stats)
  }


}
