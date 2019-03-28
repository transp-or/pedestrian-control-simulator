package optimization.simulation

import java.io.File
import java.util
import java.util.concurrent.ThreadLocalRandom

import SimulatedAnnealing.Factories.SAProblem
import SimulatedAnnealing.Others.ControlledGestionLists
import com.typesafe.config.Config
import hubmodel.DES.NOMADGraphSimulator
import hubmodel.mgmt.ControlDevices
import hubmodel.ped.PedestrianNOMAD
import hubmodel.{createSimulation, runAndWriteResults}
import hubmodel.supply.graph.FlowGateFunctional
import optimization.bruteforce.GridSearch
import myscala.math.stats.ComputeStats

class SAGatingProblem(val range: Seq[(Double, Double)], val currentSolution: (Double, Double), config: Config) extends SAProblem with GridSearch {

  override def objectiveFunction(): Double = {

    val defaultParameters = createSimulation[PedestrianNOMAD](config).getSetupArguments

    for (i <- 0 to 50) {

      val newDevices: ControlDevices = new ControlDevices(
        defaultParameters._11.monitoredAreas.map(_.clone()),
        defaultParameters._11.amws.map(_.clone()),
        if (config.getBoolean("sim.use_flow_gates")) {
          defaultParameters._11.flowGates.map(fg => new FlowGateFunctional(fg.startVertex, fg.endVertex, fg.start, fg.end, fg.monitoredArea, { x: Double => math.max(0.0000001, currentSolution._1 + currentSolution._2 * x) }))
        } else {
          Vector()
        },
        defaultParameters._11.binaryGates.map(_.clone()),
        defaultParameters._11.flowSeparators.map(_.clone()),
        defaultParameters._11.fixedFlowSeparators
      )

      val sim = new NOMADGraphSimulator[PedestrianNOMAD](
        defaultParameters._1,
        defaultParameters._2,
        defaultParameters._3,
        defaultParameters._4,
        defaultParameters._5,
        defaultParameters._6,
        defaultParameters._7,
        defaultParameters._8.clone(newDevices),
        defaultParameters._9,
        defaultParameters._10,
        newDevices
      )

      runAndWriteResults(sim, currentSolution._1.toString + "_" + currentSolution._2.toString + "_params_", if (!config.getIsNull("output.dir")) {
        Some(config.getString("output.dir"))
      } else {
        None
      })
      System.gc()
    }

    val outputDir = new File(config.getString("output.dir"))
    if (!outputDir.exists || !outputDir.isDirectory) {
      throw new IllegalArgumentException("Output dir for files does not exist ! dir=" + config.getString("output.dir"))
    }

    val files: Map[String, List[File]] = outputDir.listFiles.filter(_.isFile).toList.groupBy(f => {
      f.getName match {
        case a if a.contains("_params_tt_") => "tt"
        case b if b.contains("_params_density_") => "density"
      }
    })

    files("tt").map(ProcessTTFile2Parameters).
      flatMap(tup => tup._3.map(t => (tup._1, tup._2, t._1._1, t._1._2, t._2))).
      groupBy(tup => (tup._1, tup._2)).
      mapValues(v => v.flatMap(_._5).stats).head._2._4

  }

  override def CGInit(i: Int): ControlledGestionLists = {
    ???
  }

  override def transformSolutionDSA(arrayList: util.ArrayList[SAProblem], i: Int): SAProblem = {
    ???
  }

  override def transformSolutionLSA(): SAProblem = {
    ???
  }

  override def printSolution(s: String): Unit = {
    ???
  }

  override def getList: util.ArrayList[AnyRef] = {
    ???
  }

}
