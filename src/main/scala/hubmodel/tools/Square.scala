package hubmodel.tools

import hubmodel.NewBetterPosition2D
import myscala.math.vector.Vector2D

class Square(val center: NewBetterPosition2D, side: Double) extends MyCellComputationTrait {

  val A: NewBetterPosition2D = center + Vector2D(-0.5*side, -0.5*side)
  val B: NewBetterPosition2D = center + Vector2D(0.5*side, -0.5*side)
  val C: NewBetterPosition2D = center + Vector2D(0.5*side, 0.5*side)
  val D: NewBetterPosition2D = center + Vector2D(-0.5*side, 0.5*side)
  val angles: List[NewBetterPosition2D] = List(A, B, C, D)
  val area: Double = side*side

  def isInside(pos: NewBetterPosition2D): Boolean = {
    val AB: NewBetterPosition2D = B - A
    val BC: NewBetterPosition2D = C - B
    val AP: NewBetterPosition2D = pos - A
    val BP: NewBetterPosition2D = pos - B
    if (0 <= (AB dot AP) && (AB dot AP) <= (AB dot AB) && 0 <= (BC dot BP) && (BC dot BP) <= (BC dot BC)) true
    else false
  }

  def xCoords: Array[Double] = Array(A.X, B.X, C.X, D.X)

  def yCoords: Array[Double] = Array(A.Y, B.Y, C.Y, D.Y)
}
