package optimization

import java.nio.file.{DirectoryStream, Files, Path, Paths}

import com.typesafe.config.Config
import hubmodel.DES.NOMADGraphSimulator
import hubmodel.mgmt.flowgate.FunctionalFormDensity
import hubmodel._
import hubmodel.ped.PedestrianNOMAD
import hubmodel.tools.IllegalSimulationInput
import myscala.math.stats.ComputeStats

import scala.collection.JavaConversions._

import scala.collection.GenIterable
import scala.collection.parallel.ForkJoinTaskSupport


package object simulation {


  /**
    * Returns the a KPI from a simulation where the gating control law is specified using a polynomial of degree four.
    * The parameters are passed in increasing order of exponent.
    *
    * @param p0 constant
    * @param p1 linear
    * @param p2 quadratic
    * @param p3 cubic
    * @param p4 power 4
    * @return
    */
  def runGatingSingleFunction(p0: Double, p1: Double, p2: Double, p3: Double, p4: Double)(config: Config): Double = {

    val defaultParameters: SimulationParametersClass = createSimulation[PedestrianNOMAD](config).getSetupArgumentsNew
    val func = new FunctionalFormDensity((d: Double) => p0 + d * p1 + d * d * p2 + d * d * d * p3 + d * d * d * d * p4)


    val demandSets: Option[Seq[(String, String)]] = if (config.getBoolean("sim.read_multiple_TF_demand_sets")) {

      if (!((Paths.get(config.getString("files.TF_demand_sets")).toString == Paths.get(config.getString("files.flows_TF")).getParent.toString) &&
        (Paths.get(config.getString("files.flows_TF")).getParent.toString == Paths.get(config.getString("files.timetable_TF")).getParent.toString))) {
        throw new IllegalSimulationInput("Directories for multiple demand sets do not match !")
      }

      val multipleDemandStream: DirectoryStream[Path] = Files.newDirectoryStream(Paths.get(config.getString("files.TF_demand_sets")), "*.json")

      val files: Vector[Path] = multipleDemandStream.toVector

      multipleDemandStream.close()

      val flowBaseName: String = Paths.get(config.getString("files.flows_TF")).getFileName.toString.replace(".json", "")
      val timetableBaseName: String = Paths.get(config.getString("files.timetable_TF")).getFileName.toString.replace(".json", "")

      try {
        if (files.size % 2 != 0) {
          throw new IllegalSimulationInput("Uneven number of files for multiple demand sets ! (" + files.size + " files found)")
        } else if (files.isEmpty) {
          throw new IllegalSimulationInput("No files for multiple demand sets !")
        } else if (files.size == 2) {
          println("Warning ! Only one set of demands used for the multiple demand inputs. ")
          Some(
            Seq((
              files.find(_.getFileName.toString.contains(flowBaseName)).get.toString,
              files.find(_.getFileName.toString.contains(timetableBaseName)).get.toString
            )
            )
          )
        } else {
          Some(
            files
              .groupBy(f => f.getFileName.getFileName.toString.split("_").last.replace(".json", ""))
              .map(grouped => (grouped._2.find(_.getFileName.toString.contains(flowBaseName)).get.toString, grouped._2.find(_.getFileName.toString.contains(timetableBaseName)).get.toString)).toVector
          )
        }
      } catch {
        case e: Exception => throw e
      }
    } else {
      None
    }

    val n: Int = if (config.getBoolean("sim.read_multiple_TF_demand_sets") && config.getInt("sim.nb_runs") > 0) {
      println(" * using " + demandSets.get.size + " different pedestrian demand sets")
      println(" * ignoring number of simulation runs")
      demandSets.get.size
    } else {
      println(" * running " + config.getInt("sim.nb_runs") + " simulations")
      config.getInt("sim.nb_runs")
    }

    val range: GenIterable[Int] = if (config.getBoolean("execution.parallel")) {
      val r = (1 to n).par
      r.tasksupport = new ForkJoinTaskSupport(new scala.concurrent.forkjoin.ForkJoinPool(config.getInt("execution.threads")))
      r
    }
    else {
      1 to n
    }

    if (n > 0) {
      range.foreach(s => {
        val sim =
          if (demandSets.isDefined) {
            throw new Exception("Possibility not yet implemented !")
            createSimulation[PedestrianNOMAD](config, Some(demandSets.get(s - 1)._1), Some(demandSets.get(s - 1)._2))
          }
          else {
            val newControlDevices = defaultParameters.controlDevices.cloneModifyFlowGates(func)

            new NOMADGraphSimulator[PedestrianNOMAD](
              defaultParameters.start,
              defaultParameters.end,
              defaultParameters.mvmtUpdate,
              defaultParameters.routeUpdate,
              defaultParameters.evaluateFrequency,
              defaultParameters.rebuildTreeInterval,
              defaultParameters.microSpace,
              defaultParameters.graph.clone(newControlDevices),
              defaultParameters.timeTable,
              defaultParameters.stop2Vertex,
              newControlDevices,
              defaultParameters.writeTrajectoryData
            )
            //createSimulation[PedestrianNOMAD](config)
          }
        runAndWriteResults(
          sim,
          config.getString("output.output_prefix") + "_", if (!config.getIsNull("output.dir")) Some(config.getString("output.dir")) else {
            None
          },
          config.getBoolean("output.write_trajectories_as_VS"),
          config.getBoolean("output.write_trajectories_as_JSON"),
          config.getBoolean("output.write_tt_4_transform")
        )
        System.gc()
      })
    } else {
      println("No more simulations to run !")
    }

    // Reads intermediate results
    val results: Vector[ResultsContainerRead] = readResults(if (!config.getIsNull("output.dir")) {
      Some(config.getString("output.dir"))
    } else {
      None
    }).toVector

    // writes statistcs about each run
    val statsPerRun = results.map(r => {
      r.tt.map(_._3).stats
    })

    statsPerRun.map(r => r._4).stats._2
  }

}
