package optimization.ALNS

import java.util.concurrent.ThreadLocalRandom

import hubmodel.control.ControlDevicePolicy
import hubmodel.control.amw.AMWPolicy
import hubmodel.prediction.{AMWFlowsFromGroundTruth, StatePrediction}
import hubmodel.prediction.state.StateGroundTruthPredicted

import scala.util.Random
import myscala.output.SeqOfSeqExtensions.SeqOfSeqWriter
import tools.Time

import myscala.math.stats.{ComputeQuantiles, ComputeStats}


trait OperatorGenerator

trait Operator {
  def xprime(x: ControlDevicePolicy): ControlDevicePolicy
}

trait RandomChange {
  val probability: Double
}

trait EngineeringChange

trait OperatorFlowData

object RandomIncreaseSpeed extends Operator with OperatorGenerator with RandomChange {
  def xprime(x: ControlDevicePolicy): ControlDevicePolicy = {
    x match {
      case amw: AMWPolicy => {AMWPolicy(amw.name, amw.start, amw.end, amw.speed + 1)}
    }
  }
  val probability: Double = 0.45
}

object RandomDecreaseSpeed extends Operator with OperatorGenerator with RandomChange {
  def xprime(x: ControlDevicePolicy): ControlDevicePolicy = {
    x match {
      case amw: AMWPolicy => {AMWPolicy(amw.name, amw.start, amw.end, amw.speed - 1)}
    }
  }
  val probability: Double = 0.45
}

object RandomChangeDirection extends Operator with OperatorGenerator with RandomChange {
  def xprime(x: ControlDevicePolicy): ControlDevicePolicy = {
    x match {
      case amw: AMWPolicy => {AMWPolicy(amw.name, amw.start, amw.end, - amw.speed)}
    }
  }
  val probability: Double = 0.1
}

object CongestionIncreaseSpeed extends Operator with EngineeringChange {
  def xprime(x: ControlDevicePolicy): ControlDevicePolicy = x
}

object CongestionDescreaseSpeed extends Operator with EngineeringChange {
  def xprime(x: ControlDevicePolicy): ControlDevicePolicy = x
}


class DirectionMatchFlow(val flowDataBySimulation: Vector[Map[(String, Int, Int), Double]], val timeIntervals: Vector[Time]) extends Operator with EngineeringChange {

  val flowData: Map[(String, Int, Int), Double] = flowDataBySimulation.flatMap(_.toVector).groupBy(_._1).view.mapValues(v => v.map(_._2).statistics.median).toMap

  def xprime(x: ControlDevicePolicy): ControlDevicePolicy = {
    x match {
      case amw: AMWPolicy => {
        val flow = flowData.filter(v => v._1._1 == amw.name && timeIntervals(v._1._3) == amw.start).maxByOption(_._2)
        if (flow.exists(f => f._1._2.sign != amw.speed.sign)) {
          amw.copy(speed = flow.get._1._2.sign * 2.0)
        }
        else {amw}
      }
      case other => other
    }
  }
}

object DirectionMatchFlow extends OperatorGenerator {
  def returnOperator(currentPredictedState: Vector[StateGroundTruthPredicted]): DirectionMatchFlow = new DirectionMatchFlow(currentPredictedState.map(_.amwFlows.aggregateFlowsByAMW), currentPredictedState.head.intervals)
}


class MinimumDurationSameDirection(allPolicy: Vector[ControlDevicePolicy]) extends Operator with EngineeringChange {

  val policy: Vector[AMWPolicy] = allPolicy.collect{case a : AMWPolicy => a}

  val directionChangesIdx: Vector[Int] = policy
    .sliding(2)
    .zipWithIndex
    .collect{case change if change._1.head.speed.sign != change._1.last.speed.sign => change._2 + 1}
    .toVector

  val blockChangeIdx: Vector[Int] = Vector(0) ++ directionChangesIdx ++ Vector(policy.size)

  val blockLengths: Vector[Int] = blockChangeIdx
    .sliding(2)
    .map(pair => pair.last - pair.head)
    .toVector

  val tmp: Double = blockLengths.map(_ / policy.size.toDouble ).map(math.pow(_, -1.0)).sum
  val probs: Vector[Double] = blockLengths
    .map(_ / policy.size.toDouble )
    .map(math.pow(_, -1.0) / tmp)
    .scanLeft(0.0)((a: Double, b: Double) =>  a + b)


  val r = ThreadLocalRandom.current().nextDouble()
  val blockToChange: Int = probs.indexWhere(_ > r)-1

  def xprime(x: ControlDevicePolicy): ControlDevicePolicy = {
    x match {
      case amw: AMWPolicy if blockChangeIdx.size > 2  => {
        val idx: Int = policy.indexWhere(w => amw.start == w.start && amw.end == w.end && amw.name == w.name)
        if (blockChangeIdx(blockToChange) <= idx && idx < blockChangeIdx(blockToChange + 1)) {
          amw.copy(speed = - amw.speed)
        } else {
          amw
        }
      }
      case other => other
    }
  }
/*
  private val length: Int = 3

  private var decisionVariables: Vector[ControlDevicePolicy] = Vector()

  def checkFeasibility(DV: Vector[ControlDevicePolicy]): Boolean = {
    decisionVariables = DV
    val amwDirection = DV.zipWithIndex.map(dv => (dv._2, dv._1.decisionVariable.sign))

    !(for (i <- 0 to amwDirection.size - length) yield {
      val window = amwDirection.slice(i, i+length)
      window.head._2 == window.last._2 && window.head._2 != window.tail.head._2
    }).contains(false)
  }

  def makeFeasible: Vector[ControlDevicePolicy] = {

    decisionVariables
      .collect({case amw: AMWPolicy => amw})
      .sliding(length)
      .map({
        case a: Vector[AMWPolicy] if a.map(_.speed.sign).distinct.size == 1 => a
        case b: Vector[AMWPolicy] if b.head.speed.sign == b.last.speed.sign && b.head.speed.sign != b.tail.head.speed.sign => Vector(b.tail.head.copy(speed = - b.tail.head.speed))
      })

    for (i <- 0 until decisionVariables.size - length ) yield {
      val window = decisionVariables.slice(i, i+length)

    }
  }*/
}

object MinimumDurationSameDirection extends OperatorGenerator {
  def returnOperator(policy: Vector[ControlDevicePolicy]): MinimumDurationSameDirection = new MinimumDurationSameDirection(policy)
}

trait Constraint {
  def checkFeasibility(DV: Vector[ControlDevicePolicy]): Boolean
  def makeFeasible: Vector[ControlDevicePolicy]
}

object SpeedUpperBound extends Constraint {

  private var numberOfIterationsWithoutFeasiblity: Int = 0

  private var DVSplit: (Vector[ControlDevicePolicy], Vector[ControlDevicePolicy]) =  (Vector(), Vector())

  def checkFeasibility(DV: Vector[ControlDevicePolicy]): Boolean = {
    DVSplit = DV.partition(dv => dv.decisionVariable <= 5.0)

    if (DVSplit._2.nonEmpty) {
      numberOfIterationsWithoutFeasiblity += 1
      false
    }
    else {true}
  }

  def makeFeasible: Vector[ControlDevicePolicy] = {
    this.DVSplit._2.map {
      case amw: AMWPolicy => amw.copy(speed = 5.0)
    } ++ this.DVSplit._1
  }
}

object SpeedLowerBound extends Constraint {

  private var numberOfIterationsWithoutFeasiblity: Int = 0

  private var DVSplit: (Vector[ControlDevicePolicy], Vector[ControlDevicePolicy]) =  (Vector(), Vector())

  def checkFeasibility(DV: Vector[ControlDevicePolicy]): Boolean = {
    DVSplit = DV.partition(dv => dv.decisionVariable >= - 5.0)
    if (DVSplit._2.nonEmpty) {
      numberOfIterationsWithoutFeasiblity += 1
      false
    }
    else {true}
  }

  def makeFeasible: Vector[ControlDevicePolicy] = {
    this.DVSplit._2.map {
      case amw: AMWPolicy => amw.copy(speed = -5.0)
    } ++ this.DVSplit._1
  }
}

class ALNS(function: StatePrediction, initialPolicy: Iterable[ControlDevicePolicy], operators: Vector[OperatorGenerator], constraints: Vector[Constraint]) {

  println("Starting optimization for simulation")
  println(" * start time = " + this.function.predictionStartTime)
  println(" * end time = " + this.function.predictionEndTime)
  println(" * decision variables interval = " + this.function.predictionInterval)


  private val x0: Vector[ControlDevicePolicy] = initialPolicy.toVector

  function.predict(x0)

  private var currentBestx: Vector[ControlDevicePolicy] = x0
  private var currentBestOF: FunctionEvaluation = function.computeObjectives
  private var currentBestStateData: Vector[StateGroundTruthPredicted] = function.getPredictedStateData.toVector

  private var currentx: Vector[ControlDevicePolicy] = x0
  private var currentOF: FunctionEvaluation = function.computeObjectives
  private var currentStateData: Vector[StateGroundTruthPredicted] = function.getPredictedStateData.toVector

  def optimalSolution: (Vector[ControlDevicePolicy], FunctionEvaluation) = (this.currentBestx, this.currentBestOF)

  private val solutions: collection.mutable.ArrayBuffer[(Int, Vector[ControlDevicePolicy], FunctionEvaluation, Boolean, FunctionEvaluation, FunctionEvaluation)] = collection.mutable.ArrayBuffer((0, currentBestx, currentBestOF, true, currentBestOF, currentOF))

  val lowerBoundRandomFraction: Double = 0.2
  val upperBoundRandomFraction: Double = 0.4
  val maxIterations: Int = 100

  if (operators.collect{case rand: RandomChange => {rand.probability}}.sum != 1.0) {
    throw new Exception("Sum of probabilities for random operators different than one.")
  }

  val sizeOfX: Int = x0.size
  val orderedIdxOfX: Vector[Int] = (0 until sizeOfX).toVector

  val randomOperatorsProbCumSum: Vector[(Double, Operator with RandomChange)] = {
    operators
      .collect{case rand: Operator with RandomChange => {rand}}
      .scanLeft(0.0)((a: Double, b: Operator with RandomChange) =>  a + b.probability)
      .zip(operators.collect{case rand: Operator with RandomChange => {rand}})
  }

  def selectRandomChange: Vector[(ControlDevicePolicy, Operator with RandomChange)] = {
    val fractionToChange: Double = ThreadLocalRandom.current.nextDouble(lowerBoundRandomFraction, upperBoundRandomFraction)
    Random.shuffle(orderedIdxOfX)
      .take((fractionToChange * sizeOfX).round.toInt)
      .map(idx => {
        val operatorSample: Double = ThreadLocalRandom.current.nextDouble()
        (x0(idx), randomOperatorsProbCumSum.takeWhile(_._1 < operatorSample).last._2)
      })
  }

  def changeSolution(x: Vector[ControlDevicePolicy], currentPredictedState: Vector[StateGroundTruthPredicted]): Vector[ControlDevicePolicy] = {

    val randomChanges: Vector[(ControlDevicePolicy, Operator with RandomChange)] = selectRandomChange

    val directionOperator = DirectionMatchFlow.returnOperator(currentPredictedState)


      val tmp = x
        .map(w => {directionOperator.xprime(w)})
        .map(dv => {
      val change: Option[(ControlDevicePolicy, Operator with RandomChange)] = randomChanges.find(rc => rc._1.start == dv.start)
      if (change.isDefined) {change.get._2.xprime(dv) }
      else {dv} })

    val directionMatchOperator = MinimumDurationSameDirection.returnOperator(tmp)

    tmp.map(w => directionMatchOperator.xprime(w))
  }

  def applyConstraints(cs: Vector[Constraint], currentSolution: Vector[ControlDevicePolicy]): Vector[ControlDevicePolicy] = {

    if (cs.size == 1) {
      cs.head.checkFeasibility(currentSolution)
      cs.head.makeFeasible
    }
    else {
      cs.head.checkFeasibility(currentSolution)
      applyConstraints(cs.tail, cs.head.makeFeasible)
    }
  }

  def acceptanceCriteriaSA(i: Int, of: FunctionEvaluation): Boolean = {
    if (of("meanTT") < this.currentOF("meanTT")) {true}
    else {
      val T: Double = -0.1/math.log(0.99 + (0.00001-0.99)*i/this.maxIterations)
      println(i, this.currentBestOF("meanTT"), of("meanTT"), this.currentOF("meanTT"), T, this.currentOF("meanTT") - of("meanTT"), math.exp((this.currentOF("meanTT") - of("meanTT"))/T), this.currentx.map(_.decisionVariable))
      math.exp((this.currentOF("meanTT") - of("meanTT"))/T) > ThreadLocalRandom.current.nextDouble()
    }
  }


  def isBestSolution(of: FunctionEvaluation): Boolean = {
    of("meanTT") < this.currentBestOF("meanTT")
  }

  def optimize(): Unit = {
    var it: Int = 1
    while (it <= maxIterations) {

      print("\r * Running simulation " + it + "/" + maxIterations)
      //System.out.print(")
      val xNewRaw = changeSolution(this.currentx, this.currentStateData)
      val xNew = applyConstraints(this.constraints, xNewRaw)
      println(this.currentx.map(_.decisionVariable), xNew.map(_.decisionVariable))
      function.predict(xNew)
      val fNew = function.computeObjectives
      //println(fNew)
      var accepted = false

      if ( acceptanceCriteriaSA(it, fNew) ) {
        this.currentOF = fNew
        this.currentx = xNew
        this.currentStateData = function.getPredictedStateData
        accepted = true
      }
      this.solutions.append((it, xNew, fNew, accepted, this.currentBestOF, this.currentOF))

      if ( isBestSolution(fNew) ) {
        this.currentBestOF = fNew
        this.currentBestx = xNew
        this.currentBestStateData = function.getPredictedStateData
      }
      it = it + 1
    }
    print("\n")
  }

  def writeSolutionToCSV(file: String, path: String = ""): Unit = {
    val objectiveHeader: Vector[String] = this.solutions.head._3.keys.toVector.sorted
    val header: Vector[String] = this.solutions.head._2.sortBy(_.start).map(_.nameToString)

    this.solutions.toVector
      .map(v => Vector(v._1, v._4) ++ objectiveHeader.map(v._3) ++ objectiveHeader.map(v._5) ++ objectiveHeader.map(v._6) ++ v._2.sortBy(_.start).map(_.decisionVariable) )
      .transpose
      .writeToCSV(file, rowNames = None, columnNames = Some(Vector("it", "accepted") ++ objectiveHeader ++ objectiveHeader.map(_ ++ "_best") ++ objectiveHeader.map(_ ++ "_current") ++ header))
  }

}
