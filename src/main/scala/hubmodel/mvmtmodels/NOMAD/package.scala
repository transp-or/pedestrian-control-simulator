package hubmodel.mvmtmodels

import hubmodel.Position
import hubmodel.supply.continuous.Wall

package object NOMAD {

  /** Finds the closest end point of a wall if the point is not on the wall
    *
    * @param point point which is not on the wall
    * @param w     wall to finds end from
    * @return closest end point of the wall to the point
    */
  private def getClosestEndPoint(point: Position, w: Wall): Position = {
    if ((w.startPoint - point).norm < (w.endPoint - point).norm) {
      w.startPoint
    }
    else {
      w.endPoint
    }
  }

  /** Find the point used to compute the repulsive effects from the wall onto a pedestrian.
    *
    * @param pos Position of the pedestrian
    * @param w   wall to calculate repulsion from
    * @return position used to calculate repulsion
    */
  def getClosestPoint(pos: Position, w: Wall): Position = {
    val wallDir: Position = (w.endPoint - w.startPoint).normalized

    val proj: Position = {
      val AP: Position = pos - w.startPoint
      wallDir * (wallDir dot AP) // + w.startPoint
    }
    if (proj.dot(wallDir) > 0.0 && proj.norm <= (w.endPoint - w.startPoint).norm) proj + w.startPoint //computeProjection(pos, w)
    //if (!isOnWall(proj, w)) getClosestEndPoint(pos, w)
    else getClosestEndPoint(pos, w)
  }

}
