package optimization.bruteforce

import java.io.File

import com.typesafe.config.Config
import hubmodel.DES.SFGraphSimulator
import hubmodel.demand.PedestrianFlow_New
import hubmodel.ped.PedestrianSim
import hubmodel.{ResultsContainerNew, createSimulation, runAndCollect, runAndWriteResults}
import hubmodel.tools.cells.DensityMeasuredArea
import myscala.math.stats.ComputeStats

import scala.collection.parallel.ForkJoinTaskSupport

class FlowSensitivity(config: Config) {

  def varyOpposingFlows(increments: Double): Unit = {

    if (increments <= 0.0 || increments > 1.0) {
      throw new IllegalArgumentException("increment must be contained between 0.0 and 1.0 ! increments=" + increments)
    }
    if (config.getInt("sim.nb_runs") <= 0) {
      throw new IllegalArgumentException("repetitions must be positive ! repetitions=" + config.getInt("sim.nb_runs"))
    }

    val defaultParameters = createSimulation(config).getSetupArguments

    // checks if the output dir exists
    val outputDir = new File(config.getString("output.dir"))
    if (!outputDir.exists || !outputDir.isDirectory) {
      throw new IllegalArgumentException("Output dir for files does not exist ! dir=" + config.getString("output.dir"))
    }

    for (i <- (0.0 to 1.0 by increments).par; j <- (0.0 to 1.0 by increments).par; n <- (0 to config.getInt("sim.nb_runs")).par; if i >= j) {

      val newFlows = (
        defaultParameters._10._1.map(flow => {
          if (flow.O.ID == "bottom" && flow.D.ID == "top") {
            PedestrianFlow_New(flow.O, flow.D, flow.start, flow.end, flow.f * i)
          }
          else if (flow.O.ID == "top" && flow.D.ID == "bottom") {
            PedestrianFlow_New(flow.O, flow.D, flow.start, flow.end, flow.f * j)
          }
          else {
            throw new NoSuchElementException("this flow does not exist in this setup")
          }
        }),
        defaultParameters._10._2,
        defaultParameters._10._3
      )

      val devices = defaultParameters._11.clone()
      val sim = new SFGraphSimulator(
        defaultParameters._1,
        defaultParameters._2,
        Some(config.getString("output.log_dir")),
        defaultParameters._3,
        defaultParameters._4,
        defaultParameters._5,
        defaultParameters._6,
        defaultParameters._7.clone(devices),
        defaultParameters._8,
        defaultParameters._9,
        newFlows,
        devices
      )

      runAndWriteResults(sim, i.toString + "_" + j.toString + "_params_", config.getString("output.dir"))
      System.gc()
    }

  }

  def processWrittenResults: Map[(Double, Double), (Int, Double, Double, Double, Double, Double)] = {

    val outputDir = new File(config.getString("output.dir"))

    val files: Map[String, List[File]] = outputDir.listFiles.filter(_.isFile).toList.groupBy(f => {
      f.getName match {
        case a if a.contains("_params_tt_") => "tt"
        case b if b.contains("_params_density_") => "density"
      }
    })


files("tt").map(f => {
      val endParams: Int = f.getName.indexOf("_params_tt_")
      val params = f.getName.substring(0, endParams).split("_").map(_.toDouble).toVector
      val in = scala.io.Source.fromFile(f)
      val tt: Iterable[Double] = (for (line <- in.getLines) yield {
        val cols = line.split(",").map(_.trim)
        cols(2).toDouble
      }).toVector
      in.close
      (params(0), params(1), tt)
    }).groupBy(tup => (tup._1, tup._2)).map(tup => tup._1 -> tup._2.flatMap(_._3).stats)

 }


}


