package tools.cells

import hubmodel.{Position, generateUUID}
import myscala.math.vector.Vector2D

class Square(name: String, A: Position, B: Position, C: Position, D: Position, od: Boolean, genRate: Option[Double]) extends Rectangle(name, A, B, C, D, od, genRate) {


  /** Constructor without the name field. The name is taken from the UUID generated before hand.
    *
    * @param A
    * @param B
    * @param C
    * @param D
    * @param od
    * @param genRate
    */
  def this(A: Position, B: Position, C: Position, D: Position, od: Boolean, genRate: Option[Double]) {
    this("", A, B, C, D, od, genRate)
  }

  /** Constructor using the center and side to build the square
    *
    * @param center
    * @param side
    * @param od
    * @param genRate
    */
  def this(center: Position, side: Double, od: Boolean, genRate: Option[Double]) {
    this(
      "",
      center + Vector2D(-0.5 * side, -0.5 * side),
      center + Vector2D(0.5 * side, -0.5 * side),
      center + Vector2D(0.5 * side, 0.5 * side),
      center + Vector2D(-0.5 * side, 0.5 * side),
      od,
      genRate
    )
  }

  def this(center: Position, side: Double) {
    this(
      "",
      center + Vector2D(-0.5 * side, -0.5 * side),
      center + Vector2D(0.5 * side, -0.5 * side),
      center + Vector2D(0.5 * side, 0.5 * side),
      center + Vector2D(-0.5 * side, 0.5 * side),
      false,
      None
    )
  }
}
