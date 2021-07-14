package optimization.ALNS

import java.io.{BufferedWriter, File, FileWriter}

import hubmodel.control.{ControlDeviceData, ControlDevicePolicy}
import hubmodel.prediction.StatePrediction
import hubmodel.prediction.state.StateGroundTruthPredicted
import myscala.output.SeqOfSeqExtensions.SeqOfSeqWriter
import optimization.ALNS.constraints.Constraint
import optimization.ALNS.operators.{OperatorGenerator, RandomChange}

import scala.collection.mutable
import scala.util.Random


class ALNSPareto(f: StatePrediction,
                 xInit: Iterable[ControlDevicePolicy],
                 ops: Vector[OperatorGenerator with RandomChange],
                 cons: Vector[Constraint],
                 reduction: FunctionEvaluation => FunctionEvaluationReduced,
                 parameters: ALNSParameters = new ALNSParameters
                ) extends MetaHeuristic(f, new Policy(xInit.toVector), ops, cons, reduction) with ParetoSet {


  override val stochasticReduction: FunctionEvaluation => FunctionEvaluationReduced = reduction

  protected val weightScores: Map[String, Temperature] = parameters.weightScores
  protected val operatorWeights: collection.mutable.TreeMap[String, Double] = collection.mutable.TreeMap.from(operators.map(o => o.name -> parameters.initialScore))
  protected val maxIterations: Iteration = parameters.maxIterations

  protected val weightMin: Double = parameters.minScore
  protected val weightMax: Double = parameters.maxScore
  protected val lambda: Double = parameters.lambda

  function.predict(xInit.toVector.sorted, Vector())
  this.insert(new Policy(xInit.toVector), Vector(), function.computeObjectives, function.getPredictedStateData, Double.NaN)


  private val iterations: collection.mutable.ArrayBuffer[(Iteration, Policy, String, OperatorName, Double, FunctionEvaluationReduced, OperatorWeights)] =
    collection.mutable.ArrayBuffer((0, bestx, "accepted", "", Double.NaN, stochasticReduction(getOF(new Policy(xInit.toVector))), operatorWeights.toMap))


  // -----------------------------------------------------------------------------------------------------------------//
  // ---------------------------- METHODS ----------------------------------------------------------------------------//
  // -----------------------------------------------------------------------------------------------------------------//

  def writeIterationsToCSV(file: String, path: String = ""): Unit = {
    val objectiveHeader: Vector[String] = this.iterations.head._6.keys.toVector.sorted
    val headerDV: Vector[String] = this.iterations.head._2.x.sorted.map(_.nameToString)
    val weightHeader: Vector[String] = this.iterations.head._7.keys.toVector

    this.iterations.toVector
      .map(v => Vector(v._1, v._4, v._3, this.paretoSet.keySet.contains(v._2)) ++ v._2.x.sorted.map(_.decisionVariable) ++ objectiveHeader.map(v._6) ++ weightHeader.map(v._7) )
      .transpose
      .writeToCSV(file, rowNames = None, columnNames = Some(Vector("it", "operator", "accepted", "pareto") ++ headerDV ++ objectiveHeader ++ weightHeader.map(_ ++ "_weight")))
  }


  private val objectiveHeader: Vector[String] = this.iterations.head._6.keys.toVector.sorted
  private val headerDV: Vector[String] = this.iterations.head._2.x.sorted.map(_.nameToString)
  private val weightHeader: Vector[String] = this.iterations.head._7.keys.toVector

  val iterationsHeader: String = (Vector("it", "operator", "accepted") ++ headerDV ++ objectiveHeader ++ weightHeader.map(_ ++ "_weight")).mkString(", ")


  protected def getOF(x: Policy): FunctionEvaluation = {this.paretoSet(x)._2}

  protected def getStateData(x: Policy): Vector[StateGroundTruthPredicted] = {this.paretoSet(x)._3}

  def optimize(filePrefix: String, path: String = ""): Unit = {
    val file = new File(path + "iterations_points_" + filePrefix + ".csv")
    val bw = new BufferedWriter(new FileWriter(file))
    bw.write(iterationsHeader + "\n")

    val fileDistancePlot = new File(path + "euclidean-distance-policy-OF" + filePrefix + ".csv")
    val fdp = new BufferedWriter(new FileWriter(fileDistancePlot))

    var it: Int = 1
    while (it <= maxIterations) {
      println(" --------------------------------------------------------------------------------------------------------------------- ")
      print("\r * Running simulation " + it + "/" + maxIterations)
      println("")

      val (policy: Policy, fraction: Double) = this.selectSolution
      this.updateCurrent(policy)

      val (xNewRaw, op): (Solution, String) = changeSolution(this.currentx, this.getStateData(this.currentx), OperatorParameters(Some(fraction)))
      val xNew: Solution = (applyConstraints(this.constraints, xNewRaw._1), xNewRaw._2)

      println(" * current solution:")
      println(this.currentx.x.sortBy(p => (p.name, p.start)).map(_.decisionVariable))
      println(s" * new solution after operator: $op with fraction: $fraction")
      println(xNew._1.x.sortBy(p => (p.name, p.start)).map(_.decisionVariable))

      function.predict(xNew._1.x, xNew._2)

      if (function.computeObjectives.nonEmpty) {


        val scoreText: String = this.insert(xNew._1, xNew._2, function.computeObjectives, function.getPredictedStateData, fraction)
        val score = weightScores(scoreText)

        // updates the next solution to use a source for generation

        operatorWeights.update(op, math.max(weightMin, math.min(weightMax, operatorWeights(op) * lambda + (1.0 - lambda) * score)))

        if (scoreText == "accepted") {

          val solutionReplications: Int = this.paretoSet(xNew._1)._3.size
          if (solutionReplications > 4) {
            println("stop")
          }
          stochasticReduction(this.paretoSet(xNew._1)._2).map(kv => kv._1 + ": " + kv._2.toString).mkString(", ")
          println(s" * accepted: repl.: $solutionReplications, operator score: $scoreText, objectives: " + stochasticReduction(this.paretoSet(xNew._1)._2).map(kv => kv._1 + ": " + kv._2.toString).mkString(", "))
        } else {
          println(s" * rejected: operator score: $scoreText, objectives: " + stochasticReduction(function.computeObjectives).map(kv => kv._1 + ": " + kv._2.toString).mkString(", "))
        }

        this.iterations.append((it, xNew._1, scoreText, op, Double.NaN, stochasticReduction(function.computeObjectives), operatorWeights.toMap))
        bw.write((Vector(it, op, scoreText) ++ xNew._1.x.sorted.map(_.decisionVariable) ++ objectiveHeader.map(stochasticReduction(function.computeObjectives)) ++ weightHeader.map(operatorWeights)).mkString(", ") + "\n")
        bw.flush()

        (for (other <- this.iterations.dropRight(1)) yield {
          (
            math.pow(this.iterations.last._2.x.sorted.zip(other._2.x.sorted).map(t => math.pow(t._2.decisionVariable - t._1.decisionVariable, 2)).sum, 0.5),
            // math.abs(this.iterations.last._6("density") - other._6("density")),
            math.abs(this.iterations.last._6("meanTT") - other._6("meanTT"))
          )
        }).foreach(row => fdp.write(row._1 + ", " + row._2 + "\n"))

        fdp.flush()


        this.updateBestX(Random.shuffle(this.paretoSet.keys.toVector).head)

        it = it + 1

      print("\n")
    } else {
        println(" iteration dropped ! No simulations completed ! ")
        print("\n")
    }

    }
    bw.close()

  }




  def optimalSolution: (Policy, FunctionEvaluation, Vector[ControlDeviceData]) = {
    val tmp = this.paretoSet.map(s => (s._1, stochasticReduction(s._2._2))).minBy(_._2("meanTT"))._1
    this.updateBestX(tmp)
    (this.bestx, this.getOF(this.bestx), this.paretoSet(this.bestx)._1)
  }

  def getPoints: Vector[(Int, Policy, FunctionEvaluationReduced)] = {
    this.iterations.map(it => (it._1, it._2, it._6)).toVector
  }

}
