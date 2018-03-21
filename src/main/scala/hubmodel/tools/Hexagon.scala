package hubmodel.tools

import breeze.linalg.DenseVector
import breeze.numerics.{cos, pow, sqrt}
import hubmodel.{NewBetterPosition2D}
import myscala.math.vector.Vector2D

class Hexagon(val center: NewBetterPosition2D, edgeLength: Double) extends MyCellComputationTrait {
  val A: NewBetterPosition2D = center + Vector2D(-cos(30 * math.Pi / 180.0), sqrt(1 - pow(cos(30 * math.Pi / 180.0), 2)))*edgeLength
  val B: NewBetterPosition2D = A + Vector2D(0.0, -1.0)*edgeLength
  val C: NewBetterPosition2D = B + Vector2D(cos(30.0 * math.Pi / 180.0), -sqrt(1.0 - pow(cos(30.0 * math.Pi / 180.0), 2)))*edgeLength
  val D: NewBetterPosition2D = C + Vector2D(cos(30.0 * math.Pi / 180.0), sqrt(1.0 - pow(cos(30.0 * math.Pi / 180.0), 2)))*edgeLength
  val E: NewBetterPosition2D = D + Vector2D(0.0, 1.0)*edgeLength
  val F: NewBetterPosition2D = E + Vector2D(-cos(30.0 * math.Pi / 180.0), sqrt(1.0 - pow(cos(30.0 * math.Pi / 180.0), 2)))*edgeLength

  val angles: List[NewBetterPosition2D] = List(A, B, C, D, E, F)
  val area: Double = 1.5 * sqrt(3.0) * edgeLength * edgeLength
  /*var pedAcc: Double = 0.0
  var potential: Double = 0.0
  var stepsToFinal: Int = 0
  var updateState: Int = 0*/


  def isInside(p: NewBetterPosition2D): Boolean = {
    // https://stackoverflow.com/questions/5193331/is-a-point-inside-regular-hexagon
    val d: Double = (p - center).norm

    if (d > edgeLength) return false
    else if (d <= edgeLength * cos(30.0 * math.Pi / 180.0)) return true

    val px: Double = (p.X - center.X) * 2 / sqrt(3)
    if (px > 1.0 || px < -1.0) return false

    val py: Double = 0.5 * px + (p.Y - center.Y)
    if (py < 1.0 || py < -1.0) false
    else if (px - py < 1.0 || px - py < -1.0) false
    else true
  }

  def xCoords: Array[Double] = Array(A.X, B.X, C.X, D.X, E.X, F.X)

  def yCoords: Array[Double] = Array(A.Y, B.Y, C.Y, D.Y, E.Y, F.Y)

}
