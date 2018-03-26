package hubmodel.tools.cells

import java.util.concurrent.ThreadLocalRandom

import breeze.numerics.{cos, pow, sqrt}
import hubmodel.{Position, generateUUID}
import myscala.math.vector.Vector2D

class NewVertexHexagon(val center: Position, edgeLength: Double) extends NewVertex {
  val name: String = generateUUID

  val A: Position = center + Vector2D(-cos(30 * math.Pi / 180.0), sqrt(1 - pow(cos(30 * math.Pi / 180.0), 2)))*edgeLength
  val B: Position = A + Vector2D(0.0, -1.0)*edgeLength
  val C: Position = B + Vector2D(cos(30.0 * math.Pi / 180.0), -sqrt(1.0 - pow(cos(30.0 * math.Pi / 180.0), 2)))*edgeLength
  val D: Position = C + Vector2D(cos(30.0 * math.Pi / 180.0), sqrt(1.0 - pow(cos(30.0 * math.Pi / 180.0), 2)))*edgeLength
  val E: Position = D + Vector2D(0.0, 1.0)*edgeLength
  val F: Position = E + Vector2D(-cos(30.0 * math.Pi / 180.0), sqrt(1.0 - pow(cos(30.0 * math.Pi / 180.0), 2)))*edgeLength

  val corners: List[Position] = List(A, B, C, D, E, F)
  val area: Double = 1.5 * sqrt(3.0) * edgeLength * edgeLength
  /*var pedAcc: Double = 0.0
  var potential: Double = 0.0
  var stepsToFinal: Int = 0
  var updateState: Int = 0*/


  def isInside(p: Position): Boolean = {
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

  def uniformSamplePointInside: Position = {

    val a: Double = ThreadLocalRandom.current.nextDouble(0.0, 2.0*math.Pi)
    val r: Double = math.sqrt(ThreadLocalRandom.current.nextDouble(0.0, this.edgeLength))
    new Position(r*math.cos(a), r*math.sin(a))

  }

  //def xCoords: Array[Double] = Array(A.X, B.X, C.X, D.X, E.X, F.X)

  //def yCoords: Array[Double] = Array(A.Y, B.Y, C.Y, D.Y, E.Y, F.Y)

}
