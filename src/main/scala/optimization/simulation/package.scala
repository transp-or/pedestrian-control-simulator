package optimization


import java.nio.file.{Files, Paths}

import com.typesafe.config.Config
import hubmodel.DES.NOMADGraphSimulator
import hubmodel._
import hubmodel.demand.readDemandSets
import hubmodel.mgmt._
import hubmodel.ped.PedestrianNOMAD
import myscala.math.stats.computeQuantiles
import myscala.math.stats.{ComputeQuantiles, ComputeStats, computeBoxPlotData}

import scala.collection.GenIterable
import scala.collection.JavaConversions._
import hubmodel.DES._
import hubmodel.mgmt.flowgate.{FlowGate, FlowGateFunctional}
import hubmodel.results.{ResultsContainerRead, readResults}
import hubmodel.tools.exceptions.{ControlDevicesException, IllegalPhysicalQuantity}
import optimization.bruteforce.ParameterModifications
import org.apache.commons.math3.filter.MeasurementModel

package object simulation {

  /**
    * Returns the a KPI from a simulation where the gating control law is specified using a polynomial of degree four.
    * The parameters are passed in increasing order of exponent.
    *
    * @param p0 constant
    * @param p1 linear
    * @param p2 quadratic
    * @param rho target density
    * @return
    */
  def runGatingSingleFunction(config: Config)(p0: Double, p1: Double, p2: Double, rho: Double): Double = {

    println("--------------------- params for optim are: " + p0 + " and " + p1 + " and " + p2 + " and " + rho)

    val defaultParameters: SimulationParametersClass = createSimulation[PedestrianNOMAD](config).getSetupArgumentsNew

    val func = FunctionalFormGating((d: Density) => Flow( math.max(0.0, p0 + d.d * p1 + d.d * d.d * p2)) )


    try simulateWithCustomParameters(config, defaultParameters, ParametersForGating(func, rho)) catch {
      case e: IllegalPhysicalQuantity => {
        println(e.getMessage)
        println("Unfeasable measurement: skipping simulations")
        Double.MaxValue
      }
      case de: ControlDevicesException => {
        println(de.getMessage)
        println("Error with control device. Skipping simulations")
        Double.MaxValue
      }
      case f: Exception => { throw f }
    }
  }


  def runFlowSepFunction(config: Config)(a: Double, b: Double, c: Double, d: Double, e: Double): Double = {

    val defaultParameters: SimulationParametersClass = createSimulation[PedestrianNOMAD](config).getSetupArgumentsNew

    val func = FunctionalFormFlowSeparator( (bf: BidirectionalFlow) => SeparatorPositionFraction( (a*math.pow(bf.f1, b) + c*math.pow(bf.f2, d)) * math.pow(bf.f1+bf.f2, e) ) )

    simulateWithCustomParameters(config, defaultParameters, ParametersForFlowSeparators(func))
  }

  def simulateWithCustomParameters[T <: Measurement, U <: Output](config: Config, defaultParameters: SimulationParametersClass, func: ParameterModifications): Double = {

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
          val newControlDevices = func match {
            case fgParams: ParametersForGating[_, _] => {

              val newControlAreas = defaultParameters.controlDevices.monitoredAreas.map(_.deepCopyChangeTargetDensity(fgParams.targetDensity))

              val newFlowGates = defaultParameters.controlDevices.flowGates.map(fg => fg match {
                case fgFunc: FlowGateFunctional[_, _] => { fgFunc.deepCopy(fgParams.function) }
                case fg: FlowGate => { fg.deepCopy }
              })

              new ControlDevices(
                newControlAreas,
                defaultParameters.controlDevices.amws,
                newFlowGates,
                defaultParameters.controlDevices.binaryGates.map(_.deepCopy),
                defaultParameters.controlDevices.flowSeparators.map(_.deepCopy),
                defaultParameters.controlDevices.fixedFlowSeparators,
                None
              )

            }
            case fs: ParametersForFlowSeparators[_, _] => { defaultParameters.controlDevices.deepCopyModifyFlowSeparators(fs.function) }
          }

          val sim = new NOMADGraphSimulator[PedestrianNOMAD](
            defaultParameters.start,
            defaultParameters.end,
            defaultParameters.mvmtUpdate,
            defaultParameters.routeUpdate,
            defaultParameters.evaluateFrequency,
            defaultParameters.rebuildTreeInterval,
            defaultParameters.microSpace,
            defaultParameters.graph.deepCopy(newControlDevices),
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
      r.tt.map(_._3).cutOfAfterQuantile(99.5).statistics
    })

    statsPerRun.map(r => r.mean).statistics.mean
  }


  case class ParametersForGating[T <: Measurement, U <: Flow](function: FunctionalForm[T, U], targetDensity: Double) extends ParameterModifications
  case class ParametersForFlowSeparators[T<: Measurement, U <: SeparatorPositionFraction](function: FunctionalForm[T, U]) extends ParameterModifications

}
