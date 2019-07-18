package hubmodel.ped.History

import hubmodel.Position

case class Coordinate(private val a: Position) extends HistoryContainer{
  val pos: Position = a
  def toJSON: String = "x:" + pos.X + ", y:" + pos.Y
}
