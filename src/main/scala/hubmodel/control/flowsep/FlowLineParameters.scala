package hubmodel.control.flowsep

import hubmodel.Position

case class FlowLineParameters(name:String, start: Position, end: Position, isControlled: Int) {

  def toFlowLine: FlowLine = new FlowLine(this.name, this.start, this.end, this.isControlled)

}
