package hubmodel.tools.cells

import java.util.concurrent.ThreadLocalRandom

import hubmodel.Position
import myscala.math.vector.Vector2D

/*
import hubmodel.Position
import myscala.math.vector.Vector2D

@deprecated
class Rectangle(val center: Position, hSide: Double, vSide: Double) extends MyCellComputationTrait {

  val A: Position = center + Vector2D(-0.5*hSide, -0.5*vSide)
  val B: Position = center + Vector2D(0.5*hSide, -0.5*vSide)
  val C: Position = center + Vector2D(0.5*hSide, 0.5*vSide)
  val D: Position = center + Vector2D(-0.5*hSide, 0.5*vSide)
  val angles: List[Position] = List(A, B, C, D)
  val area: Double = hSide*vSide

  def isInside(pos: Position): Boolean = {
    val AB: Position = B - A
    val BC: Position = C - B
    val AP: Position = pos - A
    val BP: Position = pos - B
    if (0 <= (AB dot AP) && (AB dot AP) <= (AB dot AB) && 0 <= (BC dot BP) && (BC dot BP) <= (BC dot BC)) true
    else false
  }

  //def xCoords: Array[Double] = Array(A.X, B.X, C.X, D.X)

  //def yCoords: Array[Double] = Array(A.Y, B.Y, C.Y, D.Y)

}
*/
trait NewRectangle{

  def A: Position
  def B: Position
  def C: Position
  def D: Position

  // center of the rectangle
  var center: Position = A + (B - A)*0.5 + (D - A)*0.5

  // area of the associated zone
  var area: Double = (B - A).norm * (D - A).norm

  // collection of corners
  val corners: List[Position] =  List(A, B, C, D)


  /** Is the point inside the vertex ?
    *
    * @param pos [[Position]] to check
    * @return boolean indicating if the point is inside the vertex.
    */
  def isInside(pos: Position): Boolean = {
    val AB: Position = B - A
    val BC: Position = C - B
    val AP: Position = pos - A
    val BP: Position = pos - B
    if (0 <= (AB dot AP) && (AB dot AP) <= (AB dot AB) && 0 <= (BC dot BP) && (BC dot BP) <= (BC dot BC)) true
    else false
  }


  def uniformSamplePointInside: Position = {
    Vector2D(ThreadLocalRandom.current.nextDouble(A.X+0.1, B.X-0.1), ThreadLocalRandom.current.nextDouble(A.Y+0.1, D.Y-0.1))
  }
}