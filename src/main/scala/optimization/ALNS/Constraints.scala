package optimization.ALNS

import hubmodel.control.ControlDevicePolicy
import hubmodel.control.amw.AMWPolicy


trait Constraint {
  def checkFeasibility(DV: Vector[ControlDevicePolicy]): Boolean
  protected def makeFeasible: Vector[ControlDevicePolicy]

  def feasibleSolution: Vector[ControlDevicePolicy] = this.makeFeasible.sorted
}

object SpeedUpperBound extends Constraint {

  private var numberOfIterationsWithoutFeasiblity: Int = 0

  private var DVSplit: (Vector[ControlDevicePolicy], Vector[ControlDevicePolicy]) =  (Vector(), Vector())

  def checkFeasibility(DV: Vector[ControlDevicePolicy]): Boolean = {
    DVSplit = DV.partition(dv => dv.decisionVariable <= 3.0)

    if (DVSplit._2.nonEmpty) {
      numberOfIterationsWithoutFeasiblity += 1
      false
    }
    else {true}
  }

  def makeFeasible: Vector[ControlDevicePolicy] = {
    this.DVSplit._2.map {
      case amw: AMWPolicy => amw.copy(speed = 3.0)
    } ++ this.DVSplit._1
  }
}

object SpeedLowerBound extends Constraint {

  private var numberOfIterationsWithoutFeasiblity: Int = 0

  private var DVSplit: (Vector[ControlDevicePolicy], Vector[ControlDevicePolicy]) =  (Vector(), Vector())

  def checkFeasibility(DV: Vector[ControlDevicePolicy]): Boolean = {
    DVSplit = DV.partition(dv => dv.decisionVariable >= - 3.0)
    if (DVSplit._2.nonEmpty) {
      numberOfIterationsWithoutFeasiblity += 1
      false
    }
    else {true}
  }

  def makeFeasible: Vector[ControlDevicePolicy] = {
    this.DVSplit._2.map {
      case amw: AMWPolicy => amw.copy(speed = -3.0)
    } ++ this.DVSplit._1
  }
}
