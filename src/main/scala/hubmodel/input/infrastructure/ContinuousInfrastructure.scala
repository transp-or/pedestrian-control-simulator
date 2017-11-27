package hubmodel.input.infrastructure

import java.awt.geom.Path2D
import hubmodel.Position
import play.api.libs.json.{JsError, JsSuccess, JsValue, Json}

import scala.io.BufferedSource


/** Reads the infrastructure specified in terms of walls, doors, etc.
  *
  * TODO: convert this class to a function
  *
  * @param file file where the infrastructure specification is located
  */
class ContinuousSpaceReader(file: String) {

  val continuousSpace: SocialForceSpace = {
    val source: BufferedSource = scala.io.Source.fromFile(file)
    val input: JsValue = Json.parse(try source.mkString finally source.close)

    input.validate[InfraSFParser] match {
      case s: JsSuccess[InfraSFParser] => new SocialForceSpace(s.get.walls)
      case e: JsError => throw new Error("Error while parsing SF infrastructre file: " + JsError.toJson(e).toString())
    }
  }
}

/** Container of all the data linked to the "real" representation of the infrastructure. All the data
  * like the walls, location of doors, etc. is stored here.
  *
  * @param walls: Vector or [[Wall]]
  */
class SocialForceSpace(val walls: Vector[Wall]) extends Infrastructure with buildClosedPolygon {
  override val location: String = "unused"
  override val subLocation: String = "unused"
}



class InfraSFWithDoor(val walls: Vector[Wall], val doors: Vector[Doorway]) extends Infrastructure with buildClosedPolygon {
  override val location: String = "unused"
  override val subLocation: String = "unused"
  //val walls: Vector[Wall] = infraRaw.walls // must be ordered collection
  //val doors: Vector[Doorway] = infraRaw.doors

  // check connectivity of walls and builds polygon
  val polygon: Path2D = buildPolygon(walls)
}

/** Extension containing a function to build a closed [[java.awt.Polygon]] from the walls.
  * This is required to pave the space with hexagons, in the Guo2011 potential field approach.
  */
trait buildClosedPolygon {

  /** From the walls, gets the sequencs of points representing the walls. From a starting point (end of one wall),
    * find the connecting wall (excluding itself) and then gets the other end of it. This process is repeated until
    * all walls are processed. The final check is whether the final point matches the first point.
    *
    * TODO: Check whether only the outer shell needs to be used to build the polygon or if all walls will do.
    *
    * @param p current position
    * @param walls remaining walls to choose from
    * @param pSeq accumulator sequence of points
    * @return final sequence of points
    */
  def getSequencePoints(p: Position, walls: Vector[Wall], pSeq: List[Position]): List[Position] = {
    if (walls.isEmpty) pSeq
    else if (walls.size == 1) {
      assert(walls.head.startPoint == pSeq.reverse.head || walls.head.endPoint == pSeq.reverse.head)
      pSeq
    }
    else {
      walls.find(w2 => w2.startPoint == p || w2.endPoint == p) match {
        case Some(connectingWall) => {
          if (p == connectingWall.startPoint) getSequencePoints(connectingWall.endPoint, walls.dropWhile(w2 => w2.startPoint == p || w2.endPoint == p), connectingWall.endPoint :: pSeq)
          else getSequencePoints(connectingWall.startPoint, walls.dropWhile(w2 => w2.startPoint == p || w2.endPoint == p), connectingWall.startPoint :: pSeq)
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
  def buildPolygon(walls: Vector[Wall]): Path2D = {
    val polygon: Path2D = new Path2D.Double()
    val points = getSequencePoints(walls.head.endPoint, walls.tail, List(walls.head.endPoint, walls.head.startPoint)).reverse
    polygon.moveTo(points.head(0), points.head(1))
    points.tail.foreach(p => polygon.lineTo(p(0), p(1)))
    polygon
  }
}