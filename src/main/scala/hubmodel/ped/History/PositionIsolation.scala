package hubmodel.ped.History

import hubmodel.Position

import scala.math.BigDecimal.RoundingMode


case class PositionIsolation(a: Position, isolationType: Int) extends HistoryContainer {
  override val pos: Position = a
  override def toJSON: String = "\"x\":" + BigDecimal(pos.X).setScale(3, RoundingMode.HALF_UP).toString() + ", \"y\":" + BigDecimal(pos.Y).setScale(3, RoundingMode.HALF_UP).toString() + ", \"iso\":" + isolationType
}
