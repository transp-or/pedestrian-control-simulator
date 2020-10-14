package hubmodel.supply.continuous

import hubmodel.Position
import hubmodel.supply.Infrastructure

/** Container of all the data linked to the "real" representation of the infrastructure. All the data
  * like the walls, location of doors, etc. is stored here.
  *
  * @param walls : Vector or [[hubmodel.supply.continuous.Wall]]
  */
class ContinuousSpace(val walls: Vector[Wall]) extends Infrastructure with BuildClosedPolygon {
  override val amwsMode: String = "unused"
  override val subLocation: String = "unused"

  override lazy val shellCollection: List[Shell] = buildShells(walls)
  val isInsideWalkableArea: Position => Boolean = pos => super.isInsideWalkableSpace(pos)

  /** Add walls to the existing [[ContinuousSpace]] object.
    *
    * @param ws walls to add
    * @return new [[ContinuousSpace]] object with extra walls
    */
  def addWalls(ws: Iterable[Wall]): ContinuousSpace = new ContinuousSpace(this.walls ++ ws)
}
