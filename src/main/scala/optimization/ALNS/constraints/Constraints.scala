package optimization.ALNS.constraints

import hubmodel.control.ControlDevicePolicy
import hubmodel.control.amw.AMWPolicy
import optimization.ALNS.Policy


trait Constraint {
  def checkFeasibility(DV: Policy): Boolean
  protected def makeFeasible: Policy

  def feasibleSolution: Policy = this.makeFeasible
}

object SpeedUpperBound extends Constraint {

  private var numberOfIterationsWithoutFeasiblity: Int = 0

  private var DVSplit: (Vector[ControlDevicePolicy], Vector[ControlDevicePolicy]) =  (Vector(), Vector())

  def checkFeasibility(DV: Policy): Boolean = {
    DVSplit = DV.x.partition(dv => dv.decisionVariable <= 3.0)

    if (DVSplit._2.nonEmpty) {
      numberOfIterationsWithoutFeasiblity += 1
      false
    }
    else {true}
  }

  def makeFeasible: Policy = {
    new Policy(this.DVSplit._2.map {
      case amw: AMWPolicy => amw.copy(speed = 3.0)
    } ++ this.DVSplit._1)
  }
}

object SpeedLowerBound extends Constraint {

  private var numberOfIterationsWithoutFeasiblity: Int = 0

  private var DVSplit: (Vector[ControlDevicePolicy], Vector[ControlDevicePolicy]) =  (Vector(), Vector())

  def checkFeasibility(DV: Policy): Boolean = {
    DVSplit = DV.x.partition(dv => dv.decisionVariable >= - 3.0)
    if (DVSplit._2.nonEmpty) {
      numberOfIterationsWithoutFeasiblity += 1
      false
    }
    else {true}
  }

  def makeFeasible: Policy = {
    new Policy(this.DVSplit._2.map {
      case amw: AMWPolicy => amw.copy(speed = -3.0)
    } ++ this.DVSplit._1)
  }
}
