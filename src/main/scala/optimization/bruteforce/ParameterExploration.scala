package optimization.bruteforce

import hubmodel.DES.SFGraphSimulator
import hubmodel.mgmt.ControlDevices
import hubmodel.{runAndCollect}
import hubmodel.supply.graph.FlowGateFunctional
import myscala.math.stats.ComputeStats

import scala.collection.immutable.NumericRange


class ParameterExploration(val referenceSimulator: SFGraphSimulator, runsPerPoint: Int) {

  def exploreFlowGateFunctionalFormLinear(constantBounds: (Double, Double), linearBounds: (Double, Double)): Iterable[(Double, Double, (Int, Double, Double, Double, Double, Double))] = {

    val defaultParameters = referenceSimulator.getSetupArguments

    val constantRange: NumericRange[Double] =  constantBounds._1 to constantBounds._2 by (constantBounds._2 - constantBounds._1)/5.0
    val linearRange: NumericRange[Double] =  linearBounds._1 to linearBounds._2 by (linearBounds._2 - linearBounds._1)/5.0

    (for (i <- constantRange; j <-linearRange) yield {

      val newDevices: ControlDevices = new ControlDevices(
        defaultParameters._11.monitoredAreas,
        defaultParameters._11.amws,
        defaultParameters._11.flowGates.map(fg => new FlowGateFunctional(fg.startVertex, fg.endVertex, fg.start, fg.end, fg.monitoredArea, {x: Double => math.max(0.0, i + j*x)})),
        defaultParameters._11.binaryGates,
        defaultParameters._11.flowSeparators
      )

      println("Running simulation with linear flow gate function: flowrate = " + i + " + " + j + "*density")


      val ttStats = collection.immutable.Vector.fill(runsPerPoint)(new SFGraphSimulator(
        defaultParameters._1,
        defaultParameters._2,
        defaultParameters._3,
        defaultParameters._4,
        defaultParameters._5,
        defaultParameters._6,
        defaultParameters._7,
        defaultParameters._8,
        defaultParameters._9,
        defaultParameters._10,
        newDevices
      )).map(runAndCollect).flatMap(r => r._1.map(_.travelTime.value)).stats

      (i, j, ttStats)
    }).toVector

  }

}
