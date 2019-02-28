package optimization


import com.typesafe.config.Config
import hubmodel.DES.NOMADGraphSimulator
import hubmodel.mgmt.flowgate._
import hubmodel._
import hubmodel.demand.readDemandSets
import hubmodel.ped.PedestrianNOMAD
import myscala.math.stats.ComputeStats

import scala.collection.GenIterable


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

    val func = FunctionalFormDensity( (d: Density) => Flow(p0 + d.d * p1 + d.d * d.d * p2 + d.d * d.d * d.d * p3 + d.d * d.d * d.d * d.d * p4) )

    val defaultParameters: SimulationParametersClass = createSimulation[PedestrianNOMAD](config).getSetupArgumentsNew

    val demandSets: Option[Seq[(String, String)]] = readDemandSets(config)

    val n: Int = computeNumberOfSimulations(config, demandSets)

    val range: GenIterable[Int] = getIteratorForSimulations(if (config.getBoolean("execution.parallel")) {
      Some(config.getInt("execution.threads"))
    } else {
      None
    }, n)

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
