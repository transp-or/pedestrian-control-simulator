package hubmodel.ped.History

import hubmodel.Position

case class CoordinateGroup(a: Position, group: Int) extends HistoryContainer {
    override val pos: Position = a
    override def toJSON: String = "x:" + pos.X + ",y:" + pos.Y + ",iso:" + group
}
