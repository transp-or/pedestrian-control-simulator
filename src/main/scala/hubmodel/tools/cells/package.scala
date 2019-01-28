package hubmodel.tools

import hubmodel.Position

package object cells {

  /** Function to check whether a [[hubmodel.Position]] is inside a [[hubmodel.tools.cells.Rectangle]]. The default Vertex is a
    * plain rectangle, and hence this function checks whether the point is inside the rectangle. For more sophisticated
    * shapes, this function must be overriden.
    *
    * @param v   vertex
    * @param pos position ot check
    * @return boolean indicating if the point is inside the vertex
    */
  def isInVertex(v: Rectangle)(pos: Position): Boolean = {
    v.isInside(pos)
  }

}
