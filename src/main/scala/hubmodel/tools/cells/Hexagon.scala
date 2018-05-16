package hubmodel.tools.cells

import java.util.concurrent.ThreadLocalRandom

import breeze.numerics.{cos, pow, sqrt}
import hubmodel.{Position, generateUUID}
import myscala.math.vector.Vector2D

class Hexagon(val center: Position, edgeLength: Double) extends Vertex {
  val name: String = generateUUID

  val A: Position = center + Vector2D(-cos(30 * math.Pi / 180.0), sqrt(1 - pow(cos(30 * math.Pi / 180.0), 2))) * edgeLength
  val B: Position = A + Vector2D(0.0, -1.0) * edgeLength
  val C: Position = B + Vector2D(cos(30.0 * math.Pi / 180.0), -sqrt(1.0 - pow(cos(30.0 * math.Pi / 180.0), 2))) * edgeLength
  val D: Position = C + Vector2D(cos(30.0 * math.Pi / 180.0), sqrt(1.0 - pow(cos(30.0 * math.Pi / 180.0), 2))) * edgeLength
  val E: Position = D + Vector2D(0.0, 1.0) * edgeLength
  val F: Position = E + Vector2D(-cos(30.0 * math.Pi / 180.0), sqrt(1.0 - pow(cos(30.0 * math.Pi / 180.0), 2))) * edgeLength

  val corners: Vector[Position] = Vector(A, B, C, D, E, F)
  val area: Double = 1.5 * sqrt(3.0) * edgeLength * edgeLength


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

    val a: Double = ThreadLocalRandom.current.nextDouble(0.0, 2.0 * math.Pi)
    val r: Double = math.sqrt(ThreadLocalRandom.current.nextDouble(0.0, this.edgeLength))
    new Position(r * math.cos(a), r * math.sin(a))
  }

  override def toString: String = "(" + this.center.X.toString + ", " +  this.center.Y.toString + ")"
}
