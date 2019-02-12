package hubmodel.tools.cells

import hubmodel.{Position, generateUUID}
import myscala.math.vector.Vector2D

class Square(name: String, A: Position, B: Position, C: Position, D: Position, od: Boolean, genRate: Option[Double]) extends Rectangle(name, A, B, C, D, od, genRate) {

  def this(center: Position, side: Double, od: Boolean, genRate: Option[Double]) {
    this(
      generateUUID,
      center + Vector2D(-0.5 * side, -0.5 * side),
      center + Vector2D(0.5 * side, -0.5 * side),
      center + Vector2D(0.5 * side, 0.5 * side),
      center + Vector2D(-0.5 * side, 0.5 * side),
      od,
      genRate
    )
  }
}
