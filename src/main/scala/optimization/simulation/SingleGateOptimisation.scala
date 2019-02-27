package optimization.simulation

import com.typesafe.config.Config
import hubmodel.DES.NOMADGraphSimulator
import hubmodel._
import hubmodel.ped.PedestrianNOMAD
import hubmodel.mgmt.flowgate.FunctionalFormDensity
import myscala.math.stats.ComputeStats


class SingleGateOptimisation(val config: Config) {

  private val defaultParameters: SimulationParametersClass = createSimulation[PedestrianNOMAD](config).getSetupArgumentsNew

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
  def runGatingSingleFunction(p0: Double, p1: Double, p2: Double, p3: Double, p4: Double)(configFile: Config): Double = {

    val func = new FunctionalFormDensity( (d: Double) => p0 + d*p1 + d*d*p2 + d*d*d*p3 + d*d*d*d*p4 )

    val newControlDevices = defaultParameters.controlDevices.cloneModifyFlowGates(func)

    val sim = new NOMADGraphSimulator[PedestrianNOMAD](
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

    runAndWriteResults(
      sim,
      config.getString("output.output_prefix") + "_", if (!config.getIsNull("output.dir")) Some(config.getString("output.dir")) else { None },
      config.getBoolean("output.write_trajectories_as_VS"),
      config.getBoolean("output.write_trajectories_as_JSON"),
      config.getBoolean("output.write_tt_4_transform")
    )

    // Reads intermediate results
    val results: Vector[ResultsContainerRead] = readResults(if (!config.getIsNull("output.dir")) {
      Some(config.getString("output.dir"))
    } else {
      None
    }).toVector

    // writes stattistcs about each run
    val statsPerRun = results.map(r => {
      r.tt.map(_._3).stats
    })

    statsPerRun.map(r => r._4).stats._2
  }

}
