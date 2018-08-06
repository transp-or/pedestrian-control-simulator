package optimization.bruteforce

import java.io.File

import com.typesafe.config.Config
import hubmodel.DES.SFGraphSimulator
import hubmodel.mgmt.ControlDevices
import hubmodel.ped.PedestrianSim
import hubmodel.{ResultsContainerNew, runAndWriteResults}
import hubmodel.supply.graph.FlowGateFunctional
import hubmodel.tools.cells.DensityMeasuredArea
import myscala.math.stats.ComputeStats

import scala.collection.immutable.NumericRange
import scala.collection.parallel.ForkJoinTaskSupport


class ParameterExploration(val referenceSimulator: SFGraphSimulator, config: Config) {

  def exploreFlowGateFunctionalFormLinear(constantBounds: (Double, Double, Int), linearBounds: (Double, Double, Int)): Unit = {

    val defaultParameters = referenceSimulator.getSetupArguments

    val constantRange: NumericRange[Double] = constantBounds._1 to constantBounds._2 by (constantBounds._2 - constantBounds._1) / constantBounds._3
    val linearRange: NumericRange[Double] = linearBounds._1 to linearBounds._2 by (linearBounds._2 - linearBounds._1) / linearBounds._3

    // checks if the output dir exists
    val outputDir = new File(config.getString("output.dir"))
    if (!outputDir.exists || !outputDir.isDirectory) {
      throw new IllegalArgumentException("Output dir for files does not exist ! dir=" + config.getString("output.dir"))
    }


    for (i <- constantRange.par; j <- linearRange.par; k <- (0 to config.getInt("sim.nb_runs")).par) {
      //Vector.fill(config.getInt("sim.nb_runs"))({

      val newDevices: ControlDevices = new ControlDevices(
        defaultParameters._11.monitoredAreas.map(_.clone()),
        defaultParameters._11.amws.map(_.clone()),
        defaultParameters._11.flowGates.map(fg => new FlowGateFunctional(fg.startVertex, fg.endVertex, fg.start, fg.end, fg.monitoredArea, { x: Double => math.max(0.0000001, i + j * x) })),
        defaultParameters._11.binaryGates.map(_.clone()),
        defaultParameters._11.flowSeparators.map(_.clone())
      )

      val sim = new SFGraphSimulator(
        defaultParameters._1,
        defaultParameters._2,
        defaultParameters._3,
        defaultParameters._4,
        defaultParameters._5,
        defaultParameters._6,
        defaultParameters._7.clone(newDevices),
        defaultParameters._8,
        defaultParameters._9,
        defaultParameters._10,
        newDevices
      )

      runAndWriteResults(sim, i.toString + "_" + j.toString + "_params_", config.getString("output.dir"))
      System.gc()
    }
  }

    // set up the parallelism level
    //sims.tasksupport = new ForkJoinTaskSupport(new java.util.concurrent.ForkJoinPool(config.getInt("execution.threads")))


    // runs the simulations and writes the travel times to individual files
    //sims.foreach(sim => (sim._1, sim._2, runAndWriteResults(sim._3, sim._1.toString + "_" + sim._2.toString + "_params_", config.getString("output.dir"))))

    def processWrittenResults: Map[(Double, Double), ((Int, Double, Double, Double, Double, Double), (Int, Double, Double, Double, Double, Double))] = {

      val outputDir = new File(config.getString("output.dir"))
      if (!outputDir.exists || !outputDir.isDirectory) {
        throw new IllegalArgumentException("Output dir for files does not exist ! dir=" + config.getString("output.dir"))
      }

      // reads the files and process the data
      val files: Map[String, List[File]] = outputDir.listFiles.filter(_.isFile).toList.groupBy(f => {
        f.getName match {
          case a if a.contains("_params_tt_") => "tt"
          case b if b.contains("_params_density_") => "density"
        }
      })

      val ttResults: Map[(Double, Double), (Int, Double, Double, Double, Double, Double)] = files("tt").map(f => {
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

      val densityResults: Map[(Double, Double), (Int, Double, Double, Double, Double, Double)] = files("density").map(f => {
        val endParams: Int = f.getName.indexOf("_params_density_")
        val params = f.getName.substring(0, endParams).split("_").map(_.toDouble).toVector
        val in = scala.io.Source.fromFile(f)
        val densities: Iterable[Iterable[Double]] = (for (line <- in.getLines) yield {
          val cols = line.split(",").map(_.trim)
          cols.map(_.toDouble).toVector
        }).toVector
        in.close
        (params(0), params(1), densities.map(ds => ds.filter(_ > 0.0)))
      }).groupBy(tup => (tup._1, tup._2)).map(tup => tup._1 -> tup._2.head._3.head.size match {
        case a if a._2 == 1 => tup._1 -> tup._2.flatMap(_._3.flatten).stats
        case _ => throw new NotImplementedError("multiple density zones for parameter exploration not implemented !")
      })

      for (ttRes <- ttResults) yield {
        densityResults.find(_._1 == ttRes._1) match {
          case Some(dRes) => ttRes._1 -> (ttRes._2, dRes._2)
          case None => ttRes._1 -> (ttRes._2, (0, Double.NaN, Double.NaN, Double.NaN, Double.NaN, Double.NaN))
        }
      }
  }

}
