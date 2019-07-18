package hubmodel.ped.History

import hubmodel.Position

abstract class HistoryContainer {
  val pos: Position
  def toJSON: String
}
