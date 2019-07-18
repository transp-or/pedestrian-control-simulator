package hubmodel.ped.History

import hubmodel.Position

case class PositionIsolation(a: Position, isolationType: Int) extends HistoryContainer {
  val pos: Position = a
  override def toJSON: String = "x:" + pos.X + ",y:" + pos.Y + ",iso:" + isolationType
}
