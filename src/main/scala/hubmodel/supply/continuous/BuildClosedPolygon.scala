package hubmodel.supply.continuous

import java.awt.geom.Path2D

import hubmodel.Position

/** Extension containing a function to build a closed [[java.awt.Polygon]] from the walls.
  * This is required to pave the space with hexagons, in the Guo2011 potential field approach.
  */
trait BuildClosedPolygon {

  def shellCollection: List[Shell]

  class Shell(val shellType: Int, points: List[Position]) {
    val polygon: Path2D = new Path2D.Double()
    polygon.moveTo(points.head.X, points.head.Y)
    points.tail.foreach(p => polygon.lineTo(p.X, p.Y))
    //polygon.closePath()
  }

  /** From the walls, gets the sequencs of points representing the walls. From a starting point (end of one wall),
    * find the connecting wall (excluding itself) and then gets the other end of it. This process is repeated until
    * all walls are processed. The final check is whether the final point matches the first point.
    *
    * TODO: Check whether only the outer shell needs to be used to build the polygon or if all walls will do.
    *
    * @param p     current position
    * @param walls remaining walls to choose from
    * @param pSeq  accumulator sequence of points
    * @return final sequence of points
    */
  /*private def getSequencePoints(p: Position, walls: Vector[Wall], pSeq: List[Position]): List[Position] = {
    if (walls.size == 1) {
      assert(walls.head.startPoint == pSeq.reverse.head || walls.head.endPoint == pSeq.reverse.head)
      pSeq
    }
    else {
      walls.find(w2 => w2.startPoint == p || w2.endPoint == p) match {
        case Some(connectingWall) => {
          println(connectingWall)
          if (p == connectingWall.startPoint) getSequencePoints(connectingWall.endPoint, walls.filterNot(w2 => w2.startPoint == p || w2.endPoint == p), connectingWall.endPoint :: pSeq)
          else getSequencePoints(connectingWall.startPoint, walls.filterNot(w2 => w2.startPoint == p || w2.endPoint == p), connectingWall.startPoint :: pSeq)
        }
        case None => throw new Exception("wall has no connections: connectivity broken at " + p + ". Check wall coordinates !")
      }
    }
  }*/

  private def collectShells(p: Position, t: WallType, nodeSeq: List[Position], walls: Vector[Wall], shellSeq: List[Shell]): List[Shell] = {
    if (p == nodeSeq.reverse.head && walls.isEmpty) {
      new Shell(t, nodeSeq) :: shellSeq
    }
    else if (p == nodeSeq.reverse.head && walls.nonEmpty) {
      collectShells(walls.head.endPoint, walls.head.wallType, List(walls.head.startPoint), walls.tail, new Shell(t, nodeSeq) :: shellSeq)
    }
    else {
      println("else", p, nodeSeq)

      walls.find(w2 => w2.startPoint == p || w2.endPoint == p) match {
        case Some(connectingWall) => {
          println(connectingWall)
          if (p == connectingWall.startPoint) {println("second if");collectShells(connectingWall.endPoint, t, connectingWall.endPoint :: nodeSeq, walls.filterNot(w2 => w2.startPoint == p || w2.endPoint == p), shellSeq)}
          else if (p == connectingWall.endPoint) {println("second else if");collectShells(connectingWall.startPoint, t, connectingWall.startPoint :: nodeSeq, walls.filterNot(w2 => w2.startPoint == p || w2.endPoint == p), shellSeq)}
          else throw new Exception("wall has no connections: connectivity broken at " + p + ". Check wall coordinates !")
        }
        case None => throw new Exception("wall has no connections: connectivity broken at " + p + ". Check wall coordinates !")
      }
    }
  }

  /** Builds a [[java.awt.geom.Path2D]] object based on the sequence of points computed from the walls.
    *
    * @param walls collection of [[Wall]]
    * @return [[Path2D]] representing the walls
    */
  def buildShells(walls: Vector[Wall]): List[Shell] = {
    val wallsFiltered = walls.filterNot(_.wallType == SINGLELINE)
    collectShells(wallsFiltered.head.endPoint, wallsFiltered.head.wallType, List(wallsFiltered.head.endPoint, wallsFiltered.head.startPoint), wallsFiltered.tail, List())
  }

  def isInsideWalkableSpace(pos: Position): Boolean = {
    shellCollection.filter(_.shellType == OUTERSHELL).forall(s => s.polygon.contains(pos.X, pos.Y))
  }
}