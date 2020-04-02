package hubmodel.supply.graph

import tools.cells.Rectangle

class MyEdgeLevelChange(start: Rectangle, end: Rectangle) extends MyEdge(start, end) {
  val _cost = 0.0

  override def deepCopy: MyEdgeLevelChange = new MyEdgeLevelChange(start, end)

}
