package hubmodel.ped

import hubmodel.{Position, Time}
import hubmodel.TimeNumeric.mkOrderingOps
import hubmodel.tools.cells.Rectangle

class PedestrianNOMAD(oZone: Rectangle, dZone: Rectangle, entryTime: Time, posO: Position, route: List[Rectangle]) extends PedestrianSim(oZone, dZone, entryTime, posO, route) {

  def isVariableStep: Boolean = {false}
  var isolationTypePed: Int = 0
  var isolationTypeObs: Int = 0
  val isInvisible: Boolean = false
  var isolationTimeObs: Time = entryTime
  var isolationTimePed: Time = entryTime


  def updatePreviousPositionAndSpeed(t: Time): Unit = { this.addHistory(t) }


}
