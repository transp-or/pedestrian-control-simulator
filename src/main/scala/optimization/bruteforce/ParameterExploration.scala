package optimization.bruteforce

import com.typesafe.config.Config
import hubmodel.DES.SFGraphSimulator
import hubmodel.mgmt.ControlDevices
import hubmodel.{ResultsContainer, createSimulation, runAndCollect}
import hubmodel.supply.graph.FlowGateFunctional
import myscala.math.stats.ComputeStats

import scala.collection.immutable.NumericRange
import scala.collection.parallel.ForkJoinTaskSupport
import scala.concurrent.Future
import scala.util.{Failure, Success}


class ParameterExploration(val referenceSimulator: SFGraphSimulator, config: Config) {

  if (config.getInt("sim.nb_runs") < config.getInt("execution.threads") ) {
    println("WARNING ! NUMBER OF RUNS SMALLER THAN NUMBER OF THREADS TO USE ! NOT EFFICIENT")
  }

  def exploreFlowGateFunctionalFormLinear(constantBounds: (Double, Double, Int), linearBounds: (Double, Double, Int)): Iterable[(Double, Double, (Int, Double, Double, Double, Double, Double))] = {

    val defaultParameters = referenceSimulator.getSetupArguments

    val constantRange: NumericRange[Double] =  constantBounds._1 to constantBounds._2 by (constantBounds._2 - constantBounds._1)/constantBounds._3
    val linearRange: NumericRange[Double] =  linearBounds._1 to linearBounds._2 by (linearBounds._2 - linearBounds._1)/linearBounds._3

    for (i <- constantRange; j <-linearRange) yield {

      val newDevices: ControlDevices = new ControlDevices(
        defaultParameters._11.monitoredAreas.map(_.clone()),
        defaultParameters._11.amws.map(_.clone()),
        defaultParameters._11.flowGates.map(fg => new FlowGateFunctional(fg.startVertex, fg.endVertex, fg.start, fg.end, fg.monitoredArea, {x: Double => math.max(0.0, i + j*x)})),
        defaultParameters._11.binaryGates.map(_.clone()),
        defaultParameters._11.flowSeparators.map(_.clone())
      )

      println("Running simulation with linear flow gate function: flowrate = " + i + " + " + j + "*density")

      val simulationCollection: collection.parallel.ParSeq[SFGraphSimulator] = collection.parallel.ParSeq.fill(config.getInt("sim.nb_runs"))(new SFGraphSimulator(
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
      simulationCollection.tasksupport = new ForkJoinTaskSupport(new java.util.concurrent.ForkJoinPool(config.getInt("execution.threads")))
      val ttStats = simulationCollection.par.map(runAndCollect).seq.toVector.flatMap(_._1.map(_.travelTime.value)).stats

      (i, j, ttStats)
    }

  }

}
