package hubmodel.supply.continuous

import hubmodel.input.JSONReaders.ContinuousSpaceParser
import myscala.math.vector.Vector2D
import play.api.libs.json.{JsError, JsSuccess, JsValue, Json}

import scala.io.BufferedSource

/** Reads the infrastructure specified in terms of walls, doors, etc.
  *
  * TODO: convert this class to a function
  *
  * @param file file where the infrastructure specification is located
  */
class ReadContinuousSpace(file: String) {

  val continuousSpace: ContinuousSpace = {
    val source: BufferedSource = scala.io.Source.fromFile(file)
    val input: JsValue = Json.parse(try source.mkString finally source.close)

    input.validate[ContinuousSpaceParser] match {
      case s: JsSuccess[ContinuousSpaceParser] => new ContinuousSpace(s.get.walls.map(w => Wall(w.comment, Vector2D(w.x1, w.y1), Vector2D(w.x2, w.y2), w.wallType)))
      case e: JsError => throw new Error("Error while parsing SF infrastructre file: " + JsError.toJson(e).toString())
    }
  }
}