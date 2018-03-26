package hubmodel.mgmt

import hubmodel.tools.cells.{NewVertexRectangleModifiable}
import hubmodel.{Position, generateUUID}


class FlowLine(val start: Position, val end: Position)

class FlowSeparator(var startA: Position,
                    var startB: Position,
                    var endA: Position,
                    var endB: Position,
                    val inflowLinesStart: Iterable[FlowLine],
                    val inflowLinesEnd: Iterable[FlowLine],
                    val associatedZones: Iterable[NewVertexRectangleModifiable]) {

  val ID: String =  generateUUID



}
