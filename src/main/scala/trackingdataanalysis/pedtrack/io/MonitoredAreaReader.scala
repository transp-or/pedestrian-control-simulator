package trackingdataanalysis.pedtrack.io

import play.api.libs.json.{JsError, JsSuccess, JsValue, Json}
import trackingdataanalysis.pedtrack.visiosafe.{FlowLineDA, NewZone}

import scala.io.BufferedSource

class MonitoredAreaReader(file: String) {

  val (monitoredArea, flowLines): (NewZone, Vector[FlowLineDA]) = {
    val source: BufferedSource = scala.io.Source.fromFile(file)
    val input: JsValue = Json.parse(try source.mkString finally source.close)

    input.validate[MonitoredZoneCollection_JSON] match {
      case s: JsSuccess[MonitoredZoneCollection_JSON] => {
        (
          new NewZone(s.get.ma.name, (s.get.ma.x1, s.get.ma.y1), (s.get.ma.x2, s.get.ma.y2), (s.get.ma.x3, s.get.ma.y3), (s.get.ma.x4, s.get.ma.y4)),
          s.get.fl.map(data => new FlowLineDA(
            data.name,  ((data.x1, data.y1), (data.x2, data.y2)),
            if (data.inflow_type == "CONTROLLED") CONTROLLED else if (data.inflow_type == "UNCONTROLLED") UNCONTROLLED else throw new IllegalArgumentException("Inflow type from JSON file is wrong ! " + data.inflow_type))
          )
        )
      }
      case e: JsError => throw new Error("Error while parsing json file: " + JsError.toJson(e).toString())
    }
  }
}
