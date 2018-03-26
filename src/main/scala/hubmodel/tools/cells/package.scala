package hubmodel.tools

import hubmodel.Position

package object cells {

  /** Function to check whether a [[hubmodel.Position]] is inside a [[hubmodel.tools.cells.RectangularVertexTrait]]. The default Vertex is a
    * plain rectangle, and hence this function checks whether the point is inside the rectangle. For more sophisticated
    * shapes, this function must be overriden.
    *
    * @param v   vertex
    * @param pos position ot check
    * @return boolean indicating if the point is inside the vertex
    */
  def isInVertex(v: RectangularVertexTrait)(pos: Position): Boolean = {
    val AB: Position = v.B - v.A
    val BC: Position = v.C - v.B
    val AP: Position = pos - v.A
    val BP: Position = pos - v.B
    if (0 <= (AB dot AP) && (AB dot AP) <= (AB dot AB) && 0 <= (BC dot BP) && (BC dot BP) <= (BC dot BC)) true
    else false
  }

}
