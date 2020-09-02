package optimization.ALNS

import java.util.concurrent.ThreadLocalRandom
import java.util.function.DoubleToLongFunction

import hubmodel.control.ControlDevicePolicy
import hubmodel.control.amw.AMWPolicy
import hubmodel.prediction.AMWFlowsFromGroundTruth
import hubmodel.prediction.state.StateGroundTruthPredicted
import tools.Time
import tools.TimeNumeric.mkOrderingOps
import myscala.math.stats.ComputeStats

import scala.collection.MapView
import scala.util.Random


trait OperatorGenerator {

  val name: String

  type T <: Operator

  def returnOperator(x: Vector[ControlDevicePolicy], iterable: Vector[StateGroundTruthPredicted]): T
}

trait Operator {
  protected def xprime(x: Vector[ControlDevicePolicy]): Vector[ControlDevicePolicy]

  def newSolution(x: Vector[ControlDevicePolicy], controlData: Map[String, Double]): Solution = enforceSpeedChangeIntoPolicy(this.xprime(x.sorted), controlData)
}

trait RandomChange {
  val probability: Double
}

class RandomIncreaseSpeed extends Operator {

  def xprime(x: Vector[ControlDevicePolicy]): Vector[ControlDevicePolicy] = {

    val fractionToChange: Int = ThreadLocalRandom.current().nextInt((x.size * 0.1).round.toInt, (x.size * 0.6).round.toInt)
    val idxToChange: Vector[Int] = Random.shuffle(x.indices.toVector).take(fractionToChange)

    val tmp = x.zipWithIndex.map {
      case (amw: AMWPolicy, idx: Int) if idxToChange.contains(idx) => {
        amw.copy(speed = amw.speed + SPEED_INCREMENT)
      }
      case (a: ControlDevicePolicy, idx: Int) => a
    }

    tmp
  }
}

object RandomIncreaseSpeed extends OperatorGenerator with RandomChange {
  val probability: Double = 0.075
  val name: String = "IncreaseSpeed"
  type T = RandomIncreaseSpeed

  def returnOperator(x: Vector[ControlDevicePolicy], iterable: Vector[StateGroundTruthPredicted]): T = new RandomIncreaseSpeed
}

class RandomDecreaseSpeed extends Operator {

  def xprime(x: Vector[ControlDevicePolicy]): Vector[ControlDevicePolicy] = {

    val fractionToChange: Int = ThreadLocalRandom.current().nextInt((x.size * 0.1).round.toInt, (x.size * 0.6).round.toInt)
    val idxToChange: Vector[Int] = Random.shuffle(x.indices.toVector).take(fractionToChange)

    val tmp = x.zipWithIndex.map {
      case (amw: AMWPolicy, idx: Int) if idxToChange.contains(idx) => {
        amw.copy(speed = amw.speed - SPEED_INCREMENT)
      }
      case (a: ControlDevicePolicy, idx: Int) => a
    }

    tmp
  }
}

object RandomDecreaseSpeed extends OperatorGenerator with RandomChange {
  val probability: Double = 0.075
  val name: String = "DecreaseSpeed"

  type T = RandomDecreaseSpeed

  def returnOperator(x: Vector[ControlDevicePolicy], iterable: Vector[StateGroundTruthPredicted]): T = new RandomDecreaseSpeed
}


class RandomChangeDirection(policySize: Int) extends Operator {

  val orderedIdxOfX: Vector[Int] = (0 until policySize).toVector

  val idxToChange: Vector[Int] = Random.shuffle(orderedIdxOfX).take(1)

  def xprime(x: Vector[ControlDevicePolicy]): Vector[ControlDevicePolicy] = {

    val tmp = x.zipWithIndex.map {
      case (amw: AMWPolicy, idx: Int) if idxToChange.contains(idx) => {
        amw.copy(speed = -amw.speed)
      }
      case (a: ControlDevicePolicy, _) => a
    }

    tmp
  }
}


object RandomChangeDirection extends OperatorGenerator with RandomChange {
  val probability: Double = 0.05
  val name: String = "ChangeOneDirection"

  type T = RandomChangeDirection

  def returnOperator(x: Vector[ControlDevicePolicy], iterable: Vector[StateGroundTruthPredicted]): T = new RandomChangeDirection(x.size)
}

class RandomIncreaseAllSpeeds extends Operator {

  def xprime(x: Vector[ControlDevicePolicy]): Vector[ControlDevicePolicy] = {
    val tmp = x.map {
      case amwP: AMWPolicy if (amwP.speed.sign > 0.0) => {
        amwP.copy(speed = amwP.speed + SPEED_INCREMENT)
      }
      case amwP: AMWPolicy if (amwP.speed.sign < 0.0) => {
        amwP.copy(speed = amwP.speed - SPEED_INCREMENT)
      }
      case a: ControlDevicePolicy => a
    }

    tmp
  }
}

object RandomIncreaseAllSpeeds extends OperatorGenerator with RandomChange {
  val probability: Double = 0.25
  val name: String = "IncreaseAllSpeeds"

  type T = RandomIncreaseAllSpeeds

  def returnOperator(x: Vector[ControlDevicePolicy], iterable: Vector[StateGroundTruthPredicted]): T = new RandomIncreaseAllSpeeds
}


class DirectionMatchFlow(val flowDataBySimulation: Vector[Map[(String, Int, Int), Double]], val timeIntervals: Vector[Time]) extends Operator {

  val flowData: Map[(String, Int, Int), Double] = flowDataBySimulation.flatMap(_.toVector).groupBy(_._1).view.mapValues(v => v.map(_._2).statistics.median).toMap

  def xprime(x: Vector[ControlDevicePolicy]): Vector[ControlDevicePolicy] = {
    val tmp = x.map {
      case amw: AMWPolicy => {
        val flow = flowData.filter(v => v._1._1 == amw.name && timeIntervals(v._1._3) == amw.start).maxByOption(_._2)
        if (flow.exists(f => f._1._2.sign != amw.speed.sign)) {
          amw.copy(speed = flow.get._1._2.sign * 3.0)
        }
        else {
          amw
        }
      }
      case other => other
    }
    tmp
  }
}

object DirectionMatchFlow extends OperatorGenerator with RandomChange {
  val probability: Double = 0.3
  val name: String = "DirectionMatchFlow"

  type T = DirectionMatchFlow

  def returnOperator(x: Vector[ControlDevicePolicy], iterable: Vector[StateGroundTruthPredicted]): T = new DirectionMatchFlow(iterable.map(_.amwFlows.aggregateFlowsByAMW), iterable.head.intervals)
}

class MinimumDurationSameDirection(allPolicy: Vector[ControlDevicePolicy]) extends Operator {

  val (amwPolicies: Map[String, Vector[AMWPolicy]], other: Vector[ControlDevicePolicy]) = groupAMWPolicies(allPolicy)

  val directionChangesIdx: MapView[String, Vector[Int]] = amwPolicies.view.mapValues(policy => policy
    .sliding(2)
    .zipWithIndex
    .collect { case change if change._1.head.speed.sign != change._1.last.speed.sign => change._2 + 1 }
    .toVector)

  val blockChangeIdx: Map[String, Vector[Int]] = amwPolicies
    .map(kv => (kv._1 -> Vector(0).appendedAll(directionChangesIdx(kv._1)).appended(amwPolicies(kv._1).size)))

  val blockProbabilities: Map[String, (Vector[Double], Int)] = blockChangeIdx.view
    .mapValues(policy => {
      val blockLengths = policy.sliding(2).map(pair => pair.last - pair.head).toVector
      val tmp: Double = blockLengths.map(_ / policy.size.toDouble).map(math.pow(_, -1.0)).sum
      val probs = blockLengths
        .map(_ / policy.size.toDouble)
        .map(math.pow(_, -1.0) / tmp)
        .scanLeft(0.0)((a: Double, b: Double) => a + b)
      val blockToChange = probs.indexWhere(_ > ThreadLocalRandom.current().nextDouble()) - 1
      (probs, blockToChange)
    }).toMap

  // Used when all same direction and randomly change block direction
  val noDirChangeData: Map[String, (Int, Boolean)] = amwPolicies.view.mapValues(policy => {
    val orderedIdxOfX: Vector[Int] = policy.indices.toVector
    val idxToChange: Int = Random.shuffle(orderedIdxOfX.dropRight(1)).head
    val changeFirstBlock: Boolean = ThreadLocalRandom.current().nextBoolean()
    (idxToChange, changeFirstBlock)
  }).toMap


  def xprime(x: Vector[ControlDevicePolicy]): Vector[ControlDevicePolicy] = {
    val tmp = x.map {
      case amw: AMWPolicy => {
        if (blockChangeIdx(amw.name).size > 2) {
          val idx: Int = amwPolicies(amw.name).indexWhere(w => amw.start == w.start && amw.end == w.end && amw.name == w.name)
          if (blockChangeIdx(amw.name)(blockProbabilities(amw.name)._2) <= idx && idx < blockChangeIdx(amw.name)(blockProbabilities(amw.name)._2 + 1)) {
            amw.copy(speed = -amw.speed)
          } else {
            amw
          }
        }
        else if (blockChangeIdx(amw.name).size == 2) {
          val idx: Int = amwPolicies(amw.name).indexWhere(w => amw.start == w.start && amw.end == w.end && amw.name == w.name)
          if (noDirChangeData(amw.name)._2 && idx <= noDirChangeData(amw.name)._1) {
            amw.copy(speed = -amw.speed.sign * 2.0)
          }
          else if (!noDirChangeData(amw.name)._2 && noDirChangeData(amw.name)._1 < idx) {
            amw.copy(speed = -amw.speed.sign * 2.0)
          }
          else {
            amw
          }
        }
        else {
          amw
        }
      }
      case other: ControlDevicePolicy => {
        other
      }
    }
    tmp
  }
}

object MinimumDurationSameDirection extends OperatorGenerator with RandomChange {
  val probability: Double = 0.3
  val name: String = "ChangeDirectionBlock"

  type T = MinimumDurationSameDirection

  def returnOperator(x: Vector[ControlDevicePolicy], iterable: Vector[StateGroundTruthPredicted]): T = new MinimumDurationSameDirection(x)
}


class DownstreamDensityUpdate(amwAreas: Map[String, (Vector[String], Vector[String])], densityDataBySimulation: Vector[Map[(String, Int), Double]], val timeIntervals: Vector[Time]) extends Operator {

  val densityData/*: Map[(String, Int), Double]*/ = densityDataBySimulation
    .flatten
    .groupBy(_._1)
    .view
    .mapValues(v => v.map(_._2).sum/v.size.toDouble).to(Map).toVector
    .map(kv => (kv._1._1, timeIntervals.sliding(2).indexWhere(i => i.head <= timeIntervals(kv._1._2) && timeIntervals(kv._1._2) <= i.last)) -> kv._2*2.5)
    .groupBy(_._1)
    .mapValues(v => v.map(_._2).sum).to(Map)


  def xprime(x: Vector[ControlDevicePolicy]): Vector[ControlDevicePolicy] = {
    val tmp = x.map {
      case amw: AMWPolicy => {
        val startDensity = densityData.find(t => t._1 == (amwAreas(amw.name)._1.head, timeIntervals.indexOf(amw.start)))
        val endDensity = densityData.find(t => t._1 == (amwAreas(amw.name)._2.head, timeIntervals.indexOf(amw.start)))
        if (startDensity.isDefined && endDensity.isDefined && startDensity.get._2 >= endDensity.get._2) {
          amw.copy(speed = 3.0)
        }
        else if (startDensity.isDefined && endDensity.isDefined && startDensity.get._2 < endDensity.get._2) {
          amw.copy(speed = -3.0)
        }
        else if (startDensity.isDefined && endDensity.isEmpty) {
          amw.copy(speed = 3.0)
        }
        else if (startDensity.isEmpty && endDensity.isDefined) {
          amw.copy(speed = -3.0)
        }
        else {
          amw.copy(speed = 3.0)
        }
      }
      case other => other
    }
    tmp
  }
}

object DownstreamDensityUpdate extends OperatorGenerator with RandomChange {
  val probability: Double = 0.3
  val name: String = "DownstreamDensityUpdate"

  type T = DownstreamDensityUpdate

  def returnOperator(x: Vector[ControlDevicePolicy], iterable: Vector[StateGroundTruthPredicted]): T = new DownstreamDensityUpdate(iterable.head.densitiesInsideAreas.amwsZones, iterable.map(_.densitiesInsideAreas.quantile75DensityByArea), iterable.head.intervals)
}

/** Randomly set constant speed of moving walkways.
  *
  */
class RandomSetSpeed extends Operator {

  def xprime(x: Vector[ControlDevicePolicy]): Vector[ControlDevicePolicy] = {

    val policyByAMW: Map[String, Vector[ControlDevicePolicy]] = x.groupBy(_.name)

    val amwToChange = policyByAMW.keys.filter(n => ThreadLocalRandom.current().nextDouble() > 0.5).toVector

    val possibleSpeeds: Vector[Double] = (BigDecimal(MINIMUM_SPEED) to BigDecimal(MAXIMUM_SPEED) by SPEED_INCREMENT).map(_.toDouble).toVector

    (policyByAMW.collect{
      case policy if amwToChange.contains(policy._1) => {
        val newSpeed = Random.shuffle(possibleSpeeds).head
        policy._2.collect{
          case amw: AMWPolicy => amw.copy(speed = newSpeed)
          case other => other
        }
      }
      case other => other._2
    }).flatten.toVector
  }
}

object RandomSetSpeed extends OperatorGenerator with RandomChange {
  val probability: Double = 0.075
  val name: String = "RandomSetSpeed"

  type T = RandomSetSpeed

  def returnOperator(x: Vector[ControlDevicePolicy], iterable: Vector[StateGroundTruthPredicted]): T = new RandomSetSpeed
}
