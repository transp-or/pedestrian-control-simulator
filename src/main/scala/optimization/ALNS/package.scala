package optimization

import hubmodel.control.{ControlDeviceData, ControlDevicePolicy}
import hubmodel.control.amw.{AMWPolicy, MovingWalkwayControlEvents}
import tools.Time
import tools.TimeNumeric.mkOrderingOps
import hubmodel.AMW_ACCELERATION_AMPLITUDE

import scala.annotation.tailrec


package object ALNS {

  type Iteration = Int
  type Temperature = Double
  type OperatorName = String

  type FunctionEvaluation = Map[String, Vector[Double]]

  type FunctionEvaluationReduced = Map[String, Double]

  type Solution = (Vector[ControlDevicePolicy], Vector[ControlDeviceData])

  type AMWSpeedIdx = (String, Int)

  type AMWSpeed = (AMWSpeedIdx, Double)

  type ALNSPoint = Vector[AMWSpeed]

  type AMWSpeedRange = Set[Double]

  val SPEED_INCREMENT: Double = 0.5

  def findDirectionChanges(x: Vector[AMWPolicy]): Vector[Int] = {
    x.sliding(2)
      .zipWithIndex
      .collect { case change if (
        change._1.head.speed.sign > 0 && change._1.last.speed.sign < 0) ||
        (change._1.head.speed.sign < 0 && change._1.last.speed.sign > 0) /*&&
        math.abs(change._1.head.speed) + math.abs(change._1.last.speed) > ((change._1.last.end - change._1.last.start).value.toDouble*AMW_ACCELERATION_AMPLITUDE)*/ => change._2 + 1
      }
      .toVector
  }

  def groupAMWPolicies(x: Vector[ControlDevicePolicy]): (Map[String, Vector[AMWPolicy]], Vector[ControlDevicePolicy]) = {
    val (amw, others) = x.partitionMap {
      case amw: AMWPolicy => Left(amw)
      case other: ControlDevicePolicy => Right(other)
    }

    val amwPolicies: Map[String, Vector[AMWPolicy]] = amw
      .groupBy(_.name)
      .view.mapValues(_.sortBy(_.start)).to(Map)

    (amwPolicies, others)
  }

  def roundToSpeedValues(x: Double): Double = {
    (x * 4.0).round / 4.0
  }

  def enforceSpeedChangeIntoPolicy(x: Vector[ControlDevicePolicy], initialAMWSpeed: Map[String, Double]): (Vector[ControlDevicePolicy], Vector[MovingWalkwayControlEvents]) = {

    @tailrec def rec(directionChanges: Vector[Int], policy: Vector[AMWPolicy], amwEvents: MovingWalkwayControlEvents): (Vector[AMWPolicy], MovingWalkwayControlEvents) = {

      def changeAMWPolicy(dirChange: AMWPolicy, amwTripTime: Time, speedChangeStart: Time, speedChangeEnd: Time, oldSpeed: Double, newSpeed: Double)(x: AMWPolicy, previousPolicySpeed: Option[Double]): AMWPolicy = {

        val close: Option[Time] = if (dirChange.start == x.start && dirChange.end == x.end) {
          Some(dirChange.start)
        } else {
          None
        }
        val open: Option[Time] = if (dirChange.start == x.start && dirChange.end == x.end) {
          Some(dirChange.start + amwTripTime + Time(math.abs(oldSpeed / AMW_ACCELERATION_AMPLITUDE)))
        } else {
          None
        }

        x match {
          // delay to allow pedestrian to leave the amw completely spans over the policy interval.
          case amw: AMWPolicy if amw.name == dirChange.name && amw.end <= speedChangeStart && amw.speed.sign != oldSpeed.sign && amw.start >= dirChange.start => {
            amw.copy(speed = previousPolicySpeed.get)
          }
          // before speed changed as started
          case amw: AMWPolicy if amw.name == dirChange.name && amw.end <= speedChangeStart => {
            amw
          }
          // after change speed has ended
          case amw: AMWPolicy if amw.name == dirChange.name && amw.start >= speedChangeEnd => {
            amw
          }
          // interval when the speed changes ends
          case amw: AMWPolicy if amw.name == dirChange.name && amw.start < speedChangeEnd && amw.end > speedChangeEnd => {
            amw.copy(speed = newSpeed)
          }
          // interval where the speed changes begins
          case amw: AMWPolicy if amw.name == dirChange.name && (amw.start < speedChangeStart && speedChangeStart < amw.end) && amw.end >= speedChangeStart => {
            amw.copy(speed = oldSpeed + roundToSpeedValues(hubmodel.AMW_ACCELERATION_AMPLITUDE * (amw.end - speedChangeStart).value.toDouble) * (dirChange.speed.sign))
          }
          // intervals for which the speed change is happening accross all interval
          case amw: AMWPolicy if amw.name == dirChange.name && amw.start > speedChangeStart && amw.start < speedChangeEnd && amw.end < speedChangeEnd => {
            amw.copy(speed = previousPolicySpeed.get + roundToSpeedValues(hubmodel.AMW_ACCELERATION_AMPLITUDE * (amw.end - amw.start).value.toDouble) * (dirChange.speed.sign))
          }
          // non affected control policies
          case other: AMWPolicy => {
            other
          }
        }
      }


      @tailrec def processPolicy(changeAMWPolicy: (AMWPolicy, Option[Double]) => AMWPolicy)(tmpPolicy: Vector[AMWPolicy], idxToProcess: Int): Vector[AMWPolicy] = {

        if (idxToProcess == tmpPolicy.size) {
          tmpPolicy
        } else {
          val changedPolicy: Vector[AMWPolicy] = tmpPolicy.take(idxToProcess) ++ Vector(changeAMWPolicy(tmpPolicy(idxToProcess), if (idxToProcess - 1 >= 0) {
            Some(tmpPolicy(idxToProcess - 1).speed)
          } else {
            Some(initialAMWSpeed(tmpPolicy(idxToProcess).name))
          })) ++ tmpPolicy.takeRight(tmpPolicy.size - 1 - idxToProcess)
          processPolicy(changeAMWPolicy)(changedPolicy, idxToProcess + 1)
        }
      }


      if (directionChanges.isEmpty) {
        (policy, amwEvents)
      } else {
        val dirChange = policy(directionChanges.head)
        val oldSpeed = if (directionChanges.head > 0) {
          policy(directionChanges.head - 1).speed
        } else {
          initialAMWSpeed(dirChange.name)
        }
        val newSpeed: Double = dirChange.speed
        val amwTripTime: Time = Time(dirChange.amwLength / (math.abs(oldSpeed) + 1.34))
        val speedChangeStart: Time = dirChange.start + amwTripTime
        val speedChangeEnd: Time = dirChange.start + amwTripTime + Time(math.abs((newSpeed - oldSpeed) / hubmodel.AMW_ACCELERATION_AMPLITUDE))

        val updatePolicy = changeAMWPolicy(dirChange, amwTripTime, speedChangeStart, speedChangeEnd, oldSpeed, newSpeed) _
        val updateWholePolicy = processPolicy(updatePolicy) _


        val newPolicy: Vector[AMWPolicy] = updateWholePolicy(policy, 0)

        rec(findDirectionChanges(newPolicy).filter(_ > directionChanges.head + 1), newPolicy, amwEvents.copy(closeTime = amwEvents.closeTime :+ dirChange.start, openTime = amwEvents.openTime :+ dirChange.start + amwTripTime + Time(math.abs(oldSpeed / AMW_ACCELERATION_AMPLITUDE))))
      }
    }


    val (amwPolicies, other): (Map[String, Vector[AMWPolicy]], Vector[ControlDevicePolicy]) = groupAMWPolicies(x)

    val directionChanges: Map[String, Vector[Int]] = amwPolicies
      .map(g => g._1 -> {
        if ((initialAMWSpeed(g._1).sign < 0 && g._2.head.speed.sign > 0) || (initialAMWSpeed(g._1).sign > 0 && g._2.head.speed.sign < 0)) {
          0 +: findDirectionChanges(g._2)
        } else {
          findDirectionChanges(g._2)
        }
      })

    val tmp: Map[String, (Vector[AMWPolicy], MovingWalkwayControlEvents)] = amwPolicies.map(amwP => {
      amwP._1 -> rec(directionChanges(amwP._1), amwP._2, MovingWalkwayControlEvents(amwP._1))
    })

    (
      other ++ tmp.flatMap(_._2._1), tmp.map(_._2._2).toVector
    )
  }
}
