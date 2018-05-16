package hubmodel.demand

import hubmodel.Time
import play.api.libs.json.{JsError, JsSuccess, JsValue, Json}

import scala.io.BufferedSource

class ReadDisaggDemand(fileName: String) {

  private val _pedestrians: Vector[Pedestrian_JSON] = {

    val source: BufferedSource = scala.io.Source.fromFile(fileName)
    val input: JsValue = Json.parse(try source.mkString finally source.close)

    input.validate[Vector[Pedestrian_JSON]] match {
      case s: JsSuccess[Vector[Pedestrian_JSON]] => s.get
      case e: JsError => throw new Error("Error while parsing disaggregate pedestrian: " + JsError.toJson(e).toString())
    }
  }

  val pedestrians: Iterable[(String, String, Time)] = this._pedestrians.map(p => (p.oZone, p.dZone, Time(p.entryTime)))

}


