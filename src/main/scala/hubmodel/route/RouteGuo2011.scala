package hubmodel.route

import breeze.linalg.DenseVector
import breeze.numerics.{cos, pow, sqrt}
import hubmodel._

case class MyCell(center: Position, edgeLength: Double) { //}, conn: List[String]) {
  val ID: String = generateUUID
  val A: Position = center + edgeLength * DenseVector(-cos(30 * math.Pi / 180.0), sqrt(1 - pow(cos(30 * math.Pi / 180.0), 2)))
  val B: Position = A + edgeLength * DenseVector(0.0, -1.0)
  val C: Position = B + edgeLength * DenseVector(cos(30.0 * math.Pi / 180.0), -sqrt(1.0 - pow(cos(30.0 * math.Pi / 180.0), 2)))
  val D: Position = C + edgeLength * DenseVector(cos(30.0 * math.Pi / 180.0), sqrt(1.0 - pow(cos(30.0 * math.Pi / 180.0), 2)))
  val E: Position = D + edgeLength * DenseVector(0.0, 1.0)
  val F: Position = E + edgeLength * DenseVector(-cos(30.0 * math.Pi / 180.0), sqrt(1.0 - pow(cos(30.0 * math.Pi / 180.0), 2)))

  val angles: List[Position] = List(A, B, C, D, E, F)
  val area: Double = 1.5 * sqrt(3.0) * edgeLength * edgeLength
  var pedAcc: Double = 0.0
  var potential: Double = 0.0
  var stepsToFinal: Int = 0
  var updateState: Int = 0


  def isInside(p: Position): Boolean = {
    // https://stackoverflow.com/questions/5193331/is-a-point-inside-regular-hexagon
    val d: Double = breeze.linalg.norm(p - center)

    if (d > edgeLength) return false
    else if (d <= edgeLength * cos(30.0 * math.Pi / 180.0)) return true

    val px: Double = (p(0) - center(0)) * 2 / sqrt(3)
    if (px > 1.0 || px < -1.0) return false

    val py: Double = 0.5 * px + (p(1) - center(1))
    if (py < 1.0 || py < -1.0) false
    else if (px - py < 1.0 || px - py < -1.0) false
    else true
  }

  def xCoords: Array[Double] = Array(A(0), B(0), C(0), D(0), E(0), F(0))

  def yCoords: Array[Double] = Array(A(1), B(1), C(1), D(1), E(1), F(1))

}

class RouteGuo2011(sim: PedestrianDES[PedestrianSim]) extends Action {


  override def execute(): Unit = {

  }

}
