package optimization.bruteforce

import com.typesafe.config.Config
import hubmodel.DES.SFGraphSimulator
import hubmodel.demand.PedestrianFlow_New
import hubmodel.runAndCollect
import myscala.math.stats.ComputeStats

import scala.collection.parallel.ForkJoinTaskSupport

class FlowSensitivity(refSimulator: SFGraphSimulator, config: Config) {

  def varyOpposingFlows(increments: Double): Map[(Double, Double), (Int, Double, Double, Double, Double, Double)] = {

    if (increments <= 0.0 || increments > 1.0) { throw new IllegalArgumentException("increment must be contained between 0.0 and 1.0 ! increments=" + increments) }
    if (config.getInt("sim.nb_runs") <= 0) {throw new IllegalArgumentException("repetitions must be positive ! repetitions=" + config.getInt("sim.nb_runs")) }

    val defaultParameters = refSimulator.getSetupArguments

    val sims = (for (i <- 0.75 to 1.0 by increments ; j <- 0.75 to 1.0 by increments; if i >= j ) yield {
      Vector.fill(config.getInt("sim.nb_runs"))({

        val newFlows = (
          defaultParameters._10._1.map(flow => {
          if (flow.O.ID == "bottom" && flow.D.ID == "top") { PedestrianFlow_New(flow.O, flow.D, flow.start, flow.end, flow.f*i) }
          else if (flow.O.ID == "top" && flow.D.ID == "bottom") { PedestrianFlow_New(flow.O, flow.D, flow.start, flow.end, flow.f*j) }
          else {throw new NoSuchElementException("this flow does not exist in this setup")}
        }),
          defaultParameters._10._2,
          defaultParameters._10._3
        )

        val devices = defaultParameters._11.clone()
        (i, j, new SFGraphSimulator(
          defaultParameters._1,
          defaultParameters._2,
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
        )

      })
   }).toVector.flatten.par

    sims.tasksupport = new ForkJoinTaskSupport(new java.util.concurrent.ForkJoinPool(config.getInt("execution.threads")))
    val simulationResults = sims.map(sim => (sim._1, sim._2, runAndCollect(sim._3))).seq.groupBy(tup => (tup._1, tup._2))
    simulationResults.map(tu => tu._1 -> tu._2.flatMap(r => r._3._1.map(_.travelTime.value)).stats)
  }


}


