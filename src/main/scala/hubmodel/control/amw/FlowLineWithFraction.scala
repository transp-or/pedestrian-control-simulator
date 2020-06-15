package hubmodel.control.amw

import hubmodel.Position
import hubmodel.control.flowsep.FlowLine

class FlowLineWithFraction(name:String, start: Position, end: Position, val fractionKept: Double) extends FlowLine(name, start, end, 0)
