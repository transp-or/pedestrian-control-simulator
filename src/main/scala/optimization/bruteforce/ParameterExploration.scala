package optimization.bruteforce

import com.typesafe.config.Config
import hubmodel.DES.SFGraphSimulator
import hubmodel.mgmt.ControlDevices
import hubmodel.runAndCollect
import hubmodel.supply.graph.FlowGateFunctional
import myscala.math.stats.ComputeStats

import scala.collection.immutable.NumericRange
import scala.collection.parallel.ForkJoinTaskSupport


class ParameterExploration(val referenceSimulator: SFGraphSimulator, config: Config) {

  def exploreFlowGateFunctionalFormLinear(constantBounds: (Double, Double, Int), linearBounds: (Double, Double, Int)): Map[(Double, Double), ((Int, Double, Double, Double, Double, Double), (Int, Double, Double, Double, Double, Double))] = {

    val defaultParameters = referenceSimulator.getSetupArguments

    val constantRange: NumericRange[Double] = constantBounds._1 to constantBounds._2 by (constantBounds._2 - constantBounds._1) / constantBounds._3
    val linearRange: NumericRange[Double] = linearBounds._1 to linearBounds._2 by (linearBounds._2 - linearBounds._1) / linearBounds._3

    val sims = (for (i <- constantRange; j <- linearRange) yield {
      Vector.fill(config.getInt("sim.nb_runs"))({

        val newDevices: ControlDevices = new ControlDevices(
          defaultParameters._11.monitoredAreas.map(_.clone()),
          defaultParameters._11.amws.map(_.clone()),
          defaultParameters._11.flowGates.map(fg => new FlowGateFunctional(fg.startVertex, fg.endVertex, fg.start, fg.end, fg.monitoredArea, { x: Double => math.max(0.0000001, i + j * x) })),
          defaultParameters._11.binaryGates.map(_.clone()),
          defaultParameters._11.flowSeparators.map(_.clone())
        )

        (i,j,new SFGraphSimulator(
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
        )
      })
    }).toVector.flatten.par

      //println("Running simulation with linear flow gate function: flowrate = " + i + " + " + j + "*density")
    sims.tasksupport = new ForkJoinTaskSupport(new java.util.concurrent.ForkJoinPool(config.getInt("execution.threads")))
      val simulationResults = sims.map(sim => (sim._1, sim._2, runAndCollect(sim._3))).seq.groupBy(tup => (tup._1, tup._2))
      simulationResults.map(tu => tu._1 -> (tu._2.flatMap(r => r._3._1.map(_.travelTime.value)).stats, tu._2.flatMap(r => r._3._2.values.flatMap(_.densityHistory.map(_._2))).stats))
      //val densityStats = simulationResults.map(tu => (tu._1, tu._2.flatMap(r => r._3._2.values.flatMap(_.densityHistory.map(_._2))).stats))

  }

}
