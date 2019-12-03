package optimization


import java.nio.file.{Files, Paths}

import com.typesafe.config.Config
import hubmodel.DES.{NOMADGraphSimulator, _}
import hubmodel._
import hubmodel.demand.{DemandData, DemandSet, readDemandSets}
import hubmodel.mgmt._
import hubmodel.mgmt.flowgate.{FlowGate, FlowGateFunctional}
import hubmodel.ped.PedestrianNOMAD
import hubmodel.results.{ResultsContainerRead, ResultsContainerReadNew, readResults, readResultsJson}
import hubmodel.tools.exceptions.{ControlDevicesException, IllegalPhysicalQuantity}
import myscala.math.stats.{ComputeStats, Statistics, computeQuantile}
import optimization.bruteforce.ParameterModifications

import scala.collection.GenIterable

package object simulation {

  /**
    * Returns the a KPI from a simulation where the gating control law is specified using a polynomial of degree four.
    * The parameters are passed in increasing order of exponent.
    *
    * @param p0  constant
    * @param p1  linear
    * @param p2  quadratic
    * @param rho target density
    * @return
    */
  def runGatingSingleFunction(config: Config, nbrReplications: Option[Int] = None, simDir: Option[String] = None)(p0: Double, p1: Double, p2: Double, rho: Double): (String, Map[String, Double]) = {

    println("--------------------- params for optim are: " + p0 + " and " + p1 + " and " + p2 + " and " + rho)

    val defaultParameters: SimulationParametersClass = createSimulation[PedestrianNOMAD](config).getSetupArgumentsNew

    val func = FunctionalFormGating((d: Density) => Flow(math.max(0.0, p0 + d.d * p1 + d.d * d.d * p2)))


    try simulateWithCustomParameters(config, defaultParameters, ParametersForGatingWithDensity(func, rho), nbrReplications, simDir) catch {
      case e: IllegalPhysicalQuantity => {
        println(e.getMessage)
        println("Unfeasable measurement: skipping simulations")
        ("", Map())
      }
      case de: ControlDevicesException => {
        println(de.getMessage)
        println("Error with control device. Skipping simulations")
        ("", Map())
      }
      case f: Exception => {
        throw f
      }
    }
  }

  def runGatingSingleFunctionFixedDensityThreshold(config: Config, nbrReplications: Option[Int] = None, simDir: Option[String] = None)(p0: Double, p1: Double, p2: Double): (String, Map[String, Double]) = {

    println("--------------------- params for optim are: " + p0 + " and " + p1 + " and " + p2)

    val defaultParameters: SimulationParametersClass = createSimulation[PedestrianNOMAD](config).getSetupArgumentsNew

    val func = FunctionalFormGating((d: Density) => Flow(math.max(0.0, p0 + d.d * p1 + d.d * d.d * p2)))


    try simulateWithCustomParameters(config, defaultParameters, ParametersForGating(func), nbrReplications, simDir) catch {
      case e: IllegalPhysicalQuantity => {
        println(e.getMessage)
        println("Unfeasable measurement: skipping simulations")
        ("", Map())
      }
      case de: ControlDevicesException => {
        println(de.getMessage)
        println("Error with control device. Skipping simulations")
        ("", Map())
      }
      case f: Exception => {
        throw f
      }
    }
  }


  def runFlowSepFunction(config: Config)(a: Double, b: Double, c: Double): (String, Map[String, Double]) = {

    val defaultParameters: SimulationParametersClass = createSimulation[PedestrianNOMAD](config).getSetupArgumentsNew

    val func = FunctionalFormFlowSeparator((bf: BidirectionalFlow) => SeparatorPositionFraction((a * math.pow(bf.f2, b)) * math.pow(bf.f1 + bf.f2, c)))

    simulateWithCustomParameters(config, defaultParameters, ParametersForFlowSeparators(func), simDir = None)
  }

  def simulateWithCustomParameters[T <: Measurement, U <: Output](config: Config,
                                                                  defaultParameters: SimulationParametersClass,
                                                                  func: ParameterModifications,
                                                                  nbrReplications: Option[Int] = None,
                                                                  simDir: Option[String]): (String, Map[String, Double]) = {

    val demandSets: Option[Seq[DemandData]] = readDemandSets(config)

    val n: Int = if (nbrReplications.isDefined) {
      nbrReplications.get
    } else {
      computeNumberOfSimulations(config, demandSets)
    }

    val range = if (config.getBoolean("execution.parallel")) {
      getParallelVectorForSimulations(config.getInt("execution.threads"), n)
    }
    else {
      Vector.range(0, n)
    }


    val ID: String = if (simDir.isDefined) {
      simDir.get
    } else {
      generateUUID
    }
    val outputDir: String = config.getString("output.dir") + "/sim-results-" + ID + "/"
    if (!Files.exists(Paths.get(outputDir))) {
      Files.createDirectory(Paths.get(outputDir))
    } /*else {
      Files.newDirectoryStream(Paths.get(outputDir)).toVector.foreach(f => Files.delete(f))
    }*/

    //Vector((a, b, c, d, e)).writeToCSV(outputDir + ID + "_parameters.csv")

    range.foreach(s => {
      val sim =
        if (demandSets.isDefined) {
          throw new Exception("Possibility not yet implemented !")
          createSimulation[PedestrianNOMAD](config, Some(demandSets.get(s - 1)))
        }
        else {
          val newControlDevices = func match {
            case fgParams: ParametersForGating[_, _] => {

              val newControlAreas = defaultParameters.controlDevices.monitoredAreas.map(_.deepCopy)

              val newFlowGates = defaultParameters.controlDevices.flowGates.map(fg => fg match {
                case fgFunc: FlowGateFunctional[_, _] => {
                  fgFunc.deepCopy(fgParams.function)
                }
                case fg: FlowGate => {
                  fg.deepCopy
                }
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
            case fgParams: ParametersForGatingWithDensity[_, _] => {

              val newControlAreas = defaultParameters.controlDevices.monitoredAreas.map(_.deepCopyChangeTargetDensity(fgParams.targetDensity))

              val newFlowGates = defaultParameters.controlDevices.flowGates.map(fg => fg match {
                case fgFunc: FlowGateFunctional[_, _] => {
                  fgFunc.deepCopy(fgParams.function)
                }
                case fg: FlowGate => {
                  fg.deepCopy
                }
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
            case fs: ParametersForFlowSeparators[_, _] => {
              defaultParameters.controlDevices.deepCopyModifyFlowSeparators(fs.function)
            }
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

          val flows = getAggregateFlows(config)

          val (timeTable, stop2Vertex) = getPTSchedule(config)


          val disaggPopulation = getDisaggregateFlows(config)


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
    val resultsJson: Vector[ResultsContainerReadNew] = readResultsJson(outputDir, config.getString("output.output_prefix")).toVector


    // writes statistcs about each run
    val resultsFromSimulation: Map[String, Double] = func match {
      case _: ParametersForGating[_, _] => {
        val statsPerRun: Iterable[Map[Boolean, Statistics[_]]] = resultsJson.map(r => {
          r.tt
            .groupBy(_.gates.isEmpty)
            .map(g => g._1 -> g._2.map(_.tt).statistics)
        })

        val res: Map[String, Double] = Map("withGatesTTmedmed" -> statsPerRun.map(r => r(false).median).statistics.median,
          "withoutGatesTTmedmed" -> statsPerRun.map(r => r(true).median).statistics.median,
          "allPedsTTmedmed" -> resultsJson.map(r => {
            r.tt.map(_.tt).statistics
          }).map(r => r.median).statistics.median,
          "allPedsSize" -> resultsJson.map(r => r.tt.count(_.exit.isDefined)).sum,
          "withGatesTTvarmed" -> statsPerRun.map(r => r(false).variance).statistics.median,
          "withoutGatesTTvarmed" -> statsPerRun.map(r => r(true).variance).statistics.median,
          "allPedsTTvarmed" -> resultsJson.map(r => {
            r.tt.map(_.tt).statistics
          }).map(r => r.variance).statistics.median,
          "allPedsTTmed75quant" -> computeQuantile(75)(resultsJson.map(r => {
            r.tt.map(_.tt).statistics
          }).map(r => r.median)).value,
          "withGatesTTmed75quant" -> computeQuantile(75)(statsPerRun.map(r => r(false).median).toVector).value,
          "withoutGatesTTmed75quant" -> computeQuantile(75)(statsPerRun.map(r => r(true).median).toVector).value,
          "allPedsTT75quantmed" -> resultsJson.map(r => {
            computeQuantile(75)(r.tt.map(_.tt))
          }).map(r => r.value).statistics.median,
          "indDens75quantmed" -> results.map(_.monitoredAreaIndividualDensity.get).map(r => computeQuantile(75)(r.map(_._2)).value.toDouble).statistics.median,
          "indDens90quantmed" -> results.map(_.monitoredAreaIndividualDensity.get).map(r => computeQuantile(90)(r.map(_._2)).value.toDouble).statistics.median,
          "allPedsTTzones75quantmed" -> resultsJson.map(r => {
            computeQuantile(75)(r.tt.flatMap(_.ttThroughZones.map(_.tt)))
          }).map(r => r.value).statistics.median
        )

        res + ("combined-allPedsTT75quantmed-allPedsTTzones75quantmed" -> (res("allPedsTT75quantmed") / 31.0 + res("allPedsTTzones75quantmed") / 5.75))

      }
      case _: ParametersForGatingWithDensity[_, _] => {
        val statsPerRun: Iterable[Map[Boolean, Statistics[_]]] = resultsJson.map(r => {
          r.tt
            .groupBy(_.gates.isEmpty)
            .map(g => g._1 -> g._2.map(_.tt).statistics)
        })

        val res: Map[String, Double] = Map("withGatesTTmedmed" -> statsPerRun.map(r => r(false).median).statistics.median,
          "withoutGatesTTmedmed" -> statsPerRun.map(r => r(true).median).statistics.median,
          "allPedsTTmedmed" -> resultsJson.map(r => {
            r.tt.map(_.tt).statistics
          }).map(r => r.median).statistics.median,
          "allPedsSize" -> resultsJson.map(r => r.tt.count(_.exit.isDefined)).sum,
          "withGatesTTvarmed" -> statsPerRun.map(r => r(false).variance).statistics.median,
          "withoutGatesTTvarmed" -> statsPerRun.map(r => r(true).variance).statistics.median,
          "allPedsTTvarmed" -> resultsJson.map(r => {
            r.tt.map(_.tt).statistics
          }).map(r => r.variance).statistics.median,
          "allPedsTTmed75quant" -> computeQuantile(75)(resultsJson.map(r => {
            r.tt.map(_.tt).statistics
          }).map(r => r.median)).value,
          "withGatesTTmed75quant" -> computeQuantile(75)(statsPerRun.map(r => r(false).median).toVector).value,
          "withoutGatesTTmed75quant" -> computeQuantile(75)(statsPerRun.map(r => r(true).median).toVector).value,
          "allPedsTT75quantmed" -> resultsJson.map(r => {
            computeQuantile(75)(r.tt.map(_.tt))
          }).map(r => r.value).statistics.median,
          "indDens75quantmed" -> results.map(_.monitoredAreaIndividualDensity.get).map(r => computeQuantile(75)(r.map(_._2)).value.toDouble).statistics.median,
          "indDens90quantmed" -> results.map(_.monitoredAreaIndividualDensity.get).map(r => computeQuantile(90)(r.map(_._2)).value.toDouble).statistics.median,
          "allPedsTTzones75quantmed" -> resultsJson.map(r => {
            computeQuantile(75)(r.tt.flatMap(_.ttThroughZones.map(_.tt)))
          }).map(r => r.value).statistics.median
        )

        res + ("combined-allPedsTT75quantmed-allPedsTTzones75quantmed" -> (res("allPedsTT75quantmed") / 31.0 + res("allPedsTTzones75quantmed") / 5.75))
      }
      case _: ParametersForFlowSeparators[_, _] => {
        val statsPerRun = resultsJson.map(r => {
          r.tt.map(_.tt).statistics
        })
        Map("allPedsTTmedmed" -> statsPerRun.map(r => r.median).statistics.median)
      }
    }

    (ID, resultsFromSimulation)

  }

  case class ParametersForGating[T <: Measurement, U <: Flow](function: FunctionalForm[T, U]) extends ParameterModifications

  case class ParametersForGatingWithDensity[T <: Measurement, U <: Flow](function: FunctionalForm[T, U], targetDensity: Double) extends ParameterModifications

  case class ParametersForFlowSeparators[T <: Measurement, U <: SeparatorPositionFraction](function: FunctionalForm[T, U]) extends ParameterModifications

}
