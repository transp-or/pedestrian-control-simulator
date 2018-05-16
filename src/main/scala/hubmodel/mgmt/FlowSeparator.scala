package hubmodel.mgmt

import hubmodel.supply.continuous.{Wall, SINGLELINE}
import hubmodel.supply.graph.MyEdge
import hubmodel.tools.cells.RectangleModifiable
import hubmodel.{Position, generateUUID}


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

  def updateWallPosition(fraction: Double): Unit = {
    start = (startA + startB) * fraction
    end = (startA + startB) * fraction
  }

  def getWall: Wall = {
    //println(this.start, this.end)
    Wall("movable wall", this.start, this.end, SINGLELINE)
  }

}
