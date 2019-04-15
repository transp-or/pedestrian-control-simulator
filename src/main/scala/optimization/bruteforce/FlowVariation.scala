package optimization.bruteforce

import com.typesafe.config.Config
import hubmodel._
import hubmodel.DES.getFlows
import hubmodel.demand.{PedestrianFlowFunction_New, PedestrianFlowPT_New, PedestrianFlow_New}
import hubmodel.tools.Time

import scala.collection.GenIterable
import scala.collection.parallel.ForkJoinTaskSupport

class FlowVariation(flowInterval: Double, config: Config, lowerBoundFlow: Double = 1, upperBoundFlow: Double = 2) extends GridSearchNew[ParameterModificationsFlow](config) {

  private val ODs: (String, String) = ("bottom", "top")
  private val ODReversed: (String, String) = ("top", "bottom")

  def mean(data: Seq[Double]): Double = {
    data.sum / data.size
  }

  override val simulationRunsParameters: GenIterable[ParameterModificationsFlow] = if (config.getBoolean("execution.parallel")) {
    val r = (for (i <- BigDecimal(lowerBoundFlow) to BigDecimal(upperBoundFlow) by BigDecimal(flowInterval); k <- 1 to config.getInt("sim.nb_runs")) yield {
      ParameterModificationsFlow(i.toDouble)
    }).par
    r.tasksupport = new ForkJoinTaskSupport(new scala.concurrent.forkjoin.ForkJoinPool(config.getInt("execution.threads")))
    r
  } else {
    for (i <- BigDecimal(lowerBoundFlow) to BigDecimal(upperBoundFlow) by BigDecimal(flowInterval); k <- 1 to config.getInt("sim.nb_runs")) yield {
      ParameterModificationsFlow(i.toDouble)
    }
  }

  def getRunPrefix(paramMods: ParameterModificationsFlow): String = {
    paramMods.maximumFlow.toString + "_params_"
  }

  def getFlowMods(paramMods: ParameterModificationsFlow): (Iterable[PedestrianFlow_New], Iterable[PedestrianFlowPT_New], Iterable[PedestrianFlowFunction_New]) = {
    val flows = getFlows(config)

    (flows._1.map(flow => {
      if (flow.O.ID == "bottom" && flow.D.ID == "top") {
        PedestrianFlow_New(flow.O, flow.D, flow.start, flow.end, flow.f * paramMods.maximumFlow)
      }
      else if (flow.O.ID == "top" && flow.D.ID == "bottom") {
        PedestrianFlow_New(flow.O, flow.D, flow.start, flow.end, flow.f * paramMods.maximumFlow)
      }
      else {
        throw new NoSuchElementException("this flow does not exist in this setup")
      }
    }),
      flows._2,
      flows._3.map(pf => PedestrianFlowFunction_New(pf.O, pf.D, pf.start, pf.end, (t: Time) => pf.f(t) * paramMods.maximumFlow))
    )
  }

  def getParameters(paramMods: ParameterModificationsFlow): SimulatorParameters = {

    val devices = defaultParameters._11.deepCopy
    (
      defaultParameters._1,
      defaultParameters._2,
      defaultParameters._3,
      defaultParameters._4,
      defaultParameters._5,
      defaultParameters._6,
      defaultParameters._7,
      defaultParameters._8.deepCopy2AlternateGraphs(devices, 0.05),
      defaultParameters._9,
      defaultParameters._10,
      devices
    )
  }

/*
  def varyOpposingFlows(increments: Double, maxMultipler: Double = 1.0): Unit = {

    if (increments <= 0.0 || increments > maxMultipler) {
      throw new IllegalArgumentException("increment must be contained between 0.0 and 1.0 ! increments=" + increments)
    }
    if (config.getInt("sim.nb_runs") <= 0) {
      println("No simulations to perform, only reading results !") //throw new IllegalArgumentException("repetitions must be positive ! repetitions=" + config.getInt("sim.nb_runs"))
    }

    val defaultParameters = createSimulation[PedestrianNOMAD](config).getSetupArguments

    // checks if the output dir exists
    /*val outputDir = new File(config.getString("output.dir"))
    if (!outputDir.exists || !outputDir.isDirectory) {
      throw new IllegalArgumentException("Output dir for files does not exist ! dir=" + config.getString("output.dir"))
    }*/

    for (i <- (1.0 to maxMultipler by increments).par; /*j <- (0.0 to maxMultipler by increments).par;*/ n <- (1 to config.getInt("sim.nb_runs")).par /*; if i >= j*/ ) {

      val devices = defaultParameters._11.clone()
      val sim = new NOMADGraphSimulator[PedestrianNOMAD](
        defaultParameters._1,
        defaultParameters._2,
        defaultParameters._3,
        defaultParameters._4,
        defaultParameters._5,
        defaultParameters._6,
        defaultParameters._7,
        defaultParameters._8.clone(devices),
        defaultParameters._9,
        defaultParameters._10,
        devices
      )


      // Loads the pedestrian flows. These are either exogenous to the trains (from outside) or flows originating from trains.
      val flows: (Iterable[PedestrianFlow_New], Iterable[PedestrianFlowPT_New], Iterable[PedestrianFlowFunction_New]) = if (!config.getIsNull("files.flows") && config.getBoolean("sim.use_flows")) {
        readPedestrianFlows(config.getString("files.flows"))
      } else if (!config.getIsNull("files.flows_TF") && config.getBoolean("sim.use_flows")) {
        readPedestrianFlows(config.getString("files.flows_TF"))
      } else {
        println(" * using only disaggregate pedestrian demand")
        (Iterable(), Iterable(), Iterable())
      }

      val newFlows = (
        flows._1.map(flow => {
          if (flow.O.ID == "bottom" && flow.D.ID == "top") {
            PedestrianFlow_New(flow.O, flow.D, flow.start, flow.end, flow.f * i)
          }
          else if (flow.O.ID == "top" && flow.D.ID == "bottom") {
            PedestrianFlow_New(flow.O, flow.D, flow.start, flow.end, flow.f * i)
          }
          else {
            throw new NoSuchElementException("this flow does not exist in this setup")
          }
        }),
        flows._2,
        flows._3.map(pf => PedestrianFlowFunction_New(pf.O, pf.D, pf.start, pf.end, (t: Time) => pf.f(t) * i))
      )

      sim.insertEventWithZeroDelay(new ProcessPedestrianFlows(newFlows._1, newFlows._3, sim))


      runAndWriteResults(sim, i.toString + "_" + i.toString + "_params_", if (!config.getIsNull("output.dir")) {
        Some(config.getString("output.dir"))
      } else {
        None
      })
      System.gc()
    }

  }*/

  /*def processWrittenResultsSplitOD: Map[((Double, Double), String, String), ((Int, Double, Double, Double, Double, Double), (Int, Double, Double, Double, Double, Double))] = {

    val outputDir = new File(config.getString("output.dir"))

    val files: Map[String, List[File]] = outputDir.listFiles.filter(_.isFile).toList.groupBy(f => {
      f.getName match {
        case a if a.contains("_params_tt_") => "tt"
        case b if b.contains("_params_density_") => "density"
      }
    })

    files("tt").map(ProcessTTFile2Parameters).groupBy(tup => (tup._1, tup._2)).map(tup => (tup._1, ODs._1, ODs._2) -> (
      tup._2.flatMap(t => {
        t._3.getOrElse((ODs._1, ODs._2), Vector())
      }).stats,
      tup._2.flatMap(t => {
        t._3.getOrElse((ODs._2, ODs._1), Vector())
      }).stats
    )
    )
  }*/

  // checks if the output dir exists for writing the results
  /*val outputDir: Path = Paths.get(config.getString("output.dir"))
  if (!Files.exists(outputDir) || !Files.isDirectory(outputDir)) {
    Files.createDirectory(outputDir)
  }*/


  def processWrittenResults(func: Seq[Double] => Double): Map[(Double), (Iterable[Double], Iterable[Iterable[Double]])] = {
    groupResultsFiles("tt").map(ProcessTTFile1Parameter).
      flatMap(tup => tup._2.map(t => (tup._1, t._1._1, t._1._2, t._2))).
      groupBy(tup => tup._1).
      mapValues(v => (v.map(d => func(d._4)), v.map(_._4)))
  }

  def processWrittenResultsByOD(func: Seq[Double] => Double): Map[(Double), (Map[(String, String),Iterable[Double]], Iterable[Iterable[Double]])] = {
    groupResultsFiles("tt").map(ProcessTTFile1Parameter).
      flatMap(tup => tup._2.map(t => (tup._1, t._1._1, t._1._2, t._2))).
      groupBy(tup => tup._1).
      mapValues(v => {
        (
          v.groupBy(p => (p._2, p._3)).map(r => r._1 -> r._2.map(p => func(p._4))),
          v.map(_._4)
        )
      })
  }

  /*
    def processWrittenResultsOld(func: Seq[Double] => Double): Map[(Double, Double), (Iterable[Double], Iterable[Iterable[Double]])] = {
      println(config.getString("output.dir"))
      val outputDir = new File(config.getString("output.dir"))

      val files: Map[String, List[File]] = outputDir.listFiles.filter(_.isFile).toList.groupBy(f => {
        f.getName match {
          case a if a.contains("_params_tt_") => "tt"
          case b if b.contains("_params_density_") => "density"
        }
      })

      files("tt").map(ProcessTTFile2Parameters).
        flatMap(tup => tup._3.map(t => (tup._1, tup._2, t._1._1, t._1._2, t._2))).
        groupBy(tup => (tup._1, tup._2, tup._3, tup._4)).
        map(kv => (kv._1._1, kv._1._2) -> (kv._2.map(d => func(d._5)), kv._2.map(v => v._5)))
    }*/

/*
  def drawResults(results: Map[(Double, Double, String, String), (Int, Double, Double, Double, Double, Double)]): Unit = {

    val plotOptionsTT = PlotOptions(zmin = Some(28), zmax = Some(32))
    val plotOptionsVarTT = PlotOptions()

    new HeatMap(config.getString("output.output_prefix") + "_heatmap-mean-tt.png", results.map(r => (r._1._1, r._1._2, r._2._2)), "mean travel time", "bottom -> top multiplier", "top -> bottom multiplier", "Mean travel time", plotOptionsTT)
    new HeatMap(config.getString("output.output_prefix") + "_heatmap-variance-tt.png", results.map(r => (r._1._1, r._1._2, r._2._3)), "var travel time", "bottom -> top multiplier", "top -> bottom multiplier", "Variance of travel time")
    new HeatMap(config.getString("output.output_prefix") + "_heatmap-median-tt.png", results.map(r => (r._1._1, r._1._2, r._2._4)), "median travel time", "bottom -> top multiplier", "top -> bottom multiplier", "Median of travel time", plotOptionsTT)

  }


  def drawResultsSplitOD(results: Map[((Double, Double), String, String), ((Int, Double, Double, Double, Double, Double), (Int, Double, Double, Double, Double, Double))]): Unit = {

    val plotOptionsTT = PlotOptions(zmin = Some(28), zmax = Some(32))
    val plotOptionsVarTT = PlotOptions()

    new HeatMap(config.getString("output.output_prefix") + "_heatmap-mean-tt-bottom-top.png", results.map(r => (r._1._1._1, r._1._1._2, r._2._1._2)), "mean travel time", "bottom -> top multiplier", "top -> bottom multiplier", "Mean travel time from bottom to top", plotOptionsTT)
    new HeatMap(config.getString("output.output_prefix") + "_heatmap-variance-tt-bottom-top.png", results.map(r => (r._1._1._1, r._1._1._2, r._2._1._3)), "var travel time", "bottom -> top multiplier", "top -> bottom multiplier", "Variance travel time from bottom to top", plotOptionsVarTT)
    new HeatMap(config.getString("output.output_prefix") + "_heatmap-median-tt-bottom-top.png", results.map(r => (r._1._1._1, r._1._1._2, r._2._1._4)), "median travel time", "bottom -> top multiplier", "top -> bottom multiplier", "Median travel time from bottom to top", plotOptionsTT)

    new HeatMap(config.getString("output.output_prefix") + "_heatmap-mean-tt-top-bottom.png", results.map(r => (r._1._1._1, r._1._1._2, r._2._2._2)), "mean travel time", "bottom -> top multiplier", "top -> bottom multiplier", "Mean travel time from top to bottom", plotOptionsTT)
    new HeatMap(config.getString("output.output_prefix") + "_heatmap-variance-tt-top-bottom.png", results.map(r => (r._1._1._1, r._1._1._2, r._2._2._3)), "var travel time", "bottom -> top multiplier", "top -> bottom multiplier", "Variance travel time from top to bottom", plotOptionsVarTT)
    new HeatMap(config.getString("output.output_prefix") + "_heatmap-median-tt-top-bottom.png", results.map(r => (r._1._1._1, r._1._1._2, r._2._2._4)), "median travel time", "bottom -> top multiplier", "top -> bottom multiplier", "Median travel time from top to bottom", plotOptionsTT)

  }*/

  /*def drawComparisonResults(otherConfigFile: String): Unit = {

    val otherResults: (Map[(Double, Double, String, String), (Int, Double, Double, Double, Double, Double)], Map[((Double, Double), String, String), ((Int, Double, Double, Double, Double, Double), (Int, Double, Double, Double, Double, Double))]) = {
      val flowSensOther: FlowSensitivity = new FlowSensitivity(ConfigFactory.load(otherConfigFile))

      (flowSensOther.processWrittenResults(mean), flowSensOther.processWrittenResultsSplitOD)
    }


    val resultsDiff: Map[(Double, Double, String, String), (Double, Double, Double)] =
      this.processWrittenResults.map(r => r._1 -> (r._2._2 - otherResults._1(r._1)._2, r._2._3 - otherResults._1(r._1)._3, r._2._4 - otherResults._1(r._1)._4))

    val plotOptionsTT = PlotOptions(zmin = Some(-0.5), zmax = Some(0.5))
    val plotOptionsVarTT = PlotOptions(zmin = Some(-4), zmax = Some(4))

    new HeatMap(config.getString("output.output_prefix") + "_heatmap-difference-mean-tt.png", resultsDiff.map(r => (r._1._1, r._1._2, r._2._1)), "mean travel time", "bottom -> top multiplier", "top -> bottom multiplier", "Mean travel time", plotOptionsTT)
    new HeatMap(config.getString("output.output_prefix") + "_heatmap-difference-variance-tt.png", resultsDiff.map(r => (r._1._1, r._1._2, r._2._2)), "var travel time", "bottom -> top multiplier", "top -> bottom multiplier", "Variance of travel time", plotOptionsVarTT)
    new HeatMap(config.getString("output.output_prefix") + "_heatmap-difference-median-tt.png", resultsDiff.map(r => (r._1._1, r._1._2, r._2._3)), "median travel time", "bottom -> top multiplier", "top -> bottom multiplier", "Median of travel time", plotOptionsTT)

    val resultsDiffSplitOD: Map[((Double, Double), String, String), ((Double, Double, Double), (Double, Double, Double))] =
      this.processWrittenResultsSplitOD.map(r => r._1 -> ((r._2._1._2 - otherResults._2(r._1)._1._2, r._2._1._3 - otherResults._2(r._1)._1._3, r._2._1._4 - otherResults._2(r._1)._1._4), (r._2._2._2 - otherResults._2(r._1)._2._2, r._2._2._3 - otherResults._2(r._1)._2._3, r._2._2._4 - otherResults._2(r._1)._2._4)))

    new HeatMap(config.getString("output.output_prefix") + "_heatmap-difference-mean-tt-bottom-top.png", resultsDiffSplitOD.map(r => (r._1._1._1, r._1._1._2, r._2._1._1)), "mean travel time", "bottom -> top multiplier", "top -> bottom multiplier", "Mean travel time from bottom to top", plotOptionsTT)
    new HeatMap(config.getString("output.output_prefix") + "_heatmap-difference-variance-tt-bottom-top.png", resultsDiffSplitOD.map(r => (r._1._1._1, r._1._1._2, r._2._1._2)), "var travel time", "bottom -> top multiplier", "top -> bottom multiplier", "Variance travel time from bottom to top", plotOptionsVarTT)
    new HeatMap(config.getString("output.output_prefix") + "_heatmap-difference-median-tt-bottom-top.png", resultsDiffSplitOD.map(r => (r._1._1._1, r._1._1._2, r._2._1._3)), "median travel time", "bottom -> top multiplier", "top -> bottom multiplier", "Median travel time from bottom to top", plotOptionsTT)

    new HeatMap(config.getString("output.output_prefix") + "_heatmap-difference-mean-tt-top-bottom.png", resultsDiffSplitOD.map(r => (r._1._1._1, r._1._1._2, r._2._2._1)), "mean travel time", "bottom -> top multiplier", "top -> bottom multiplier", "Mean travel time from top to bottom", plotOptionsTT)
    new HeatMap(config.getString("output.output_prefix") + "_heatmap-difference-variance-tt-top-bottom.png", resultsDiffSplitOD.map(r => (r._1._1._1, r._1._1._2, r._2._2._2)), "var travel time", "bottom -> top multiplier", "top -> bottom multiplier", "Variance travel time from top to bottom", plotOptionsVarTT)
    new HeatMap(config.getString("output.output_prefix") + "_heatmap-difference-median-tt-top-bottom.png", resultsDiffSplitOD.map(r => (r._1._1._1, r._1._1._2, r._2._2._3)), "median travel time", "bottom -> top multiplier", "top -> bottom multiplier", "Median travel time from top to bottom", plotOptionsTT)
  }*/


}


