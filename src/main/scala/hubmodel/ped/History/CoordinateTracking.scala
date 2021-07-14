package hubmodel.ped.History

import hubmodel.Position

case class CoordinateTracking(private val a: Position) extends HistoryContainer{
  override val pos: Position = a
  override def toJSON: String = "\"x\":" + pos.X + ", \"y\":" + pos.Y
}
