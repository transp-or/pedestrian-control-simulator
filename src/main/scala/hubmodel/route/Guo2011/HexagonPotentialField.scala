package hubmodel.route.Guo2011

import hubmodel._
import hubmodel.supply.potential.PotentialCell
import tools.cells.{Hexagon, VertexPlotting}

class HexagonPotentialField(c: Position, sideLength: Double) extends Hexagon(c, sideLength) with VertexPlotting with PotentialCell { //}, conn: List[String]) {
  /*val ID: String = generateUUID
  val A: Position = center + edgeLength * DenseVector(-cos(30 * math.Pi / 180.0), sqrt(1 - pow(cos(30 * math.Pi / 180.0), 2)))
  val B: Position = A + edgeLength * DenseVector(0.0, -1.0)
  val C: Position = B + edgeLength * DenseVector(cos(30.0 * math.Pi / 180.0), -sqrt(1.0 - pow(cos(30.0 * math.Pi / 180.0), 2)))
  val D: Position = C + edgeLength * DenseVector(cos(30.0 * math.Pi / 180.0), sqrt(1.0 - pow(cos(30.0 * math.Pi / 180.0), 2)))
  val E: Position = D + edgeLength * DenseVector(0.0, 1.0)
  val F: Position = E + edgeLength * DenseVector(-cos(30.0 * math.Pi / 180.0), sqrt(1.0 - pow(cos(30.0 * math.Pi / 180.0), 2)))

  val angles: List[Position] = List(A, B, C, D, E, F)
  val area: Double = 1.5 * sqrt(3.0) * edgeLength * edgeLength*/
  var pedAcc: Double = 0.0
  var potential: Double = 0.0
  var stepsToFinal: Int = 0
  var updateState: Int = 0

  def scalarToShow: Double = pedAcc / area

  def stringToShow: String = {
    this.pedAcc.toString + " / " + "%1.2f".format(this.potential)
  }

  def horizontalMaxTextWidth: Double = this.sideLength

  override def canEqual(other: Any): Boolean = other.isInstanceOf[HexagonPotentialField]

  /** Checks whether another object equals this one by comparing the positions associated to the vertex
    *
    * @param other another object to test equality for
    * @return boolean indicating if the two objects are the same
    */
  override def equals(other: Any): Boolean =
    other match {
      case that: HexagonPotentialField if that.canEqual(this) => this.hashCode == that.hashCode
      case _ => false
    }

  override def hashCode: Int = (this.c, this.sideLength).##

}
