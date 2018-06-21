package hubmodel.mgmt

import hubmodel.supply.continuous.{SINGLELINE, Wall}
import hubmodel.supply.graph.MyEdge
import hubmodel.tools.cells.RectangleModifiable
import hubmodel.{Position, Time, generateUUID}


class FlowLine(val start: Position, val end: Position)

class FlowSeparator(var startA: Position,
                    var startB: Position,
                    var endA: Position,
                    var endB: Position,
                    val inflowLinesStart: Iterable[FlowLine],
                    val inflowLinesEnd: Iterable[FlowLine],
                    val associatedZonesStart: Iterable[RectangleModifiable],
                    val associatedZonesEnd: Iterable[RectangleModifiable],
                    val associatedConnectivity: Iterable[MyEdge],
                    val overridenZones: Iterable[String]) {

  val ID: String = generateUUID

  var start: Position = (startA + startB)*0.5
  var end: Position = (endA + endB)*0.5

  private val positionHistory: collection.mutable.ArrayBuffer[(Time, Position, Position)] = collection.mutable.ArrayBuffer()

  def getPositionHistory: IndexedSeq[(Time, Position, Position)] = positionHistory.toVector

  def initializePositionHistory(startTime: Time): Unit = {
    positionHistory.append((startTime, start, end))
  }

  def updateWallPosition(fraction: Double, time: Time): Unit = {
    positionHistory.append((time, start, end))
    start = (startA + startB) * fraction
    end = (startA + startB) * fraction
  }

  def getWall: Wall = {
    //println(this.start, this.end)
    Wall("movable wall", this.start, this.end, SINGLELINE)
  }

}
