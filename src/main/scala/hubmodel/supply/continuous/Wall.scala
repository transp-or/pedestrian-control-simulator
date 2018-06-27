package hubmodel.supply.continuous

import hubmodel.Position

/**
  * Representation of a wall: a line between two points. Order of points is arbitrary.
  *
  * @param startPoint one end
  * @param endPoint   other end
  */
case class Wall(comment: String, startPoint: Position, endPoint: Position, wallType: WallType) {
  val aw: Double = 10.0
}

class MovableWall(comm: String, s: Position, e: Position, t: WallType) extends Wall(comm, s, e, t)
