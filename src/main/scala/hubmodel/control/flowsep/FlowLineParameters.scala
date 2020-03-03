package hubmodel.control.flowsep

import hubmodel.Position

case class FlowLineParameters(start: Position, end: Position, isControlled: Int) {

  def toFlowLine: FlowLine = new FlowLine(this.start, this.end, this.isControlled)

}
