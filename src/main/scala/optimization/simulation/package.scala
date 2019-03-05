package optimization


import java.nio.file.{Files, Paths}

import com.typesafe.config.Config
import hubmodel.DES.NOMADGraphSimulator
import hubmodel._
import hubmodel.demand.readDemandSets
import hubmodel.mgmt.ControlDevices
import hubmodel.mgmt.flowgate._
import hubmodel.ped.PedestrianNOMAD
import myscala.math.stats.ComputeStats

import scala.collection.GenIterable
import scala.collection.JavaConversions._

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
  def runGatingSingleFunction(config: Config)(p0: Double, p1: Double, p2: Double, p3: Double, p4: Double) {

    val defaultParameters: SimulationParametersClass = createSimulation[PedestrianNOMAD](config).getSetupArgumentsNew

    val func = FunctionalFormDensity( (d: Density) => Flow(p0 + d.d * p1 + d.d * d.d * p2 + d.d * d.d * d.d * p3 + d.d * d.d * d.d * d.d * p4) )
    val newControlDevices = defaultParameters.controlDevices.cloneModifyFlowGates(func)

    simulateWithCustomParameters(config, defaultParameters, newControlDevices)
  }


  def runFlowSepFunction(config: Config)(a: Double, b: Double, c: Double, d: Double, e: Double) {

    val defaultParameters: SimulationParametersClass = createSimulation[PedestrianNOMAD](config).getSetupArgumentsNew

    val func = FunctionalFormFlowSeparator( (bf: BidirectionalFlow) => SeparatorPositionFraction( (a*math.pow(bf.f1, b) + c*math.pow(bf.f2, d)) * math.pow(bf.f1+bf.f2, e) ) )
    val newControlDevices = defaultParameters.controlDevices.cloneModifyFlowSeparators(func)

    simulateWithCustomParameters(config, defaultParameters, newControlDevices)
  }

  def simulateWithCustomParameters(config: Config, defaultParameters: SimulationParametersClass, newControlDevices: ControlDevices): Double = {

    val demandSets: Option[Seq[(String, String)]] = readDemandSets(config)

    val n: Int = computeNumberOfSimulations(config, demandSets)

    val range: GenIterable[Int] = getIteratorForSimulations(if (config.getBoolean("execution.parallel")) {
      Some(config.getInt("execution.threads"))
    } else {
      None
    }, n)

    val ID: String = generateUUID
    val outputDir: String = config.getString("output.dir") + "/sim-results-" + ID + "/"
    if (!Files.exists(Paths.get(outputDir))) {
      Files.createDirectory(Paths.get(outputDir))
    } else {
      Files.newDirectoryStream(Paths.get(outputDir)).toVector.foreach(f => Files.delete(f))
    }

    //Vector((a, b, c, d, e)).writeToCSV(outputDir + ID + "_parameters.csv")

    range.foreach(s => {
      val sim =
        if (demandSets.isDefined) {
          throw new Exception("Possibility not yet implemented !")
          createSimulation[PedestrianNOMAD](config, Some(demandSets.get(s - 1)._1), Some(demandSets.get(s - 1)._2))
        }
        else {

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

          val flows = getFlows(config)

          val (timeTable, stop2Vertex) = getPTSchedule(flows, config)


          val disaggPopulation = getDisaggPopulation(config)


          insertDemandIntoSimulator(sim, disaggPopulation, flows, timeTable)

          sim
        }

      runAndWriteResults(
        sim,
        config.getString("output.output_prefix") + "_",
        outputDir,
        config.getBoolean("output.write_trajectories_as_VS"),
        config.getBoolean("output.write_trajectories_as_JSON"),
        config.getBoolean("output.write_tt_4_transform")
      )

      System.gc()
    })


    // Reads intermediate results
    val results: Vector[ResultsContainerRead] = readResults(outputDir, config.getString("output.output_prefix")).toVector

    // writes statistcs about each run
    val statsPerRun = results.map(r => {
      r.tt.map(_._3).stats
    })

    statsPerRun.map(r => r._4).stats._2
  }

}
