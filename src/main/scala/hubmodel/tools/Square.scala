package hubmodel.tools

import hubmodel.Position
import myscala.math.vector.Vector2D

class Square(val center: Position, side: Double) extends MyCellComputationTrait {

  val A: Position = center + Vector2D(-0.5*side, -0.5*side)
  val B: Position = center + Vector2D(0.5*side, -0.5*side)
  val C: Position = center + Vector2D(0.5*side, 0.5*side)
  val D: Position = center + Vector2D(-0.5*side, 0.5*side)
  val angles: List[Position] = List(A, B, C, D)
  val area: Double = side*side

  def isInside(pos: Position): Boolean = {
    val AB: Position = B - A
    val BC: Position = C - B
    val AP: Position = pos - A
    val BP: Position = pos - B
    if (0 <= (AB dot AP) && (AB dot AP) <= (AB dot AB) && 0 <= (BC dot BP) && (BC dot BP) <= (BC dot BC)) true
    else false
  }

  def xCoords: Array[Double] = Array(A.X, B.X, C.X, D.X)

  def yCoords: Array[Double] = Array(A.Y, B.Y, C.Y, D.Y)
}
