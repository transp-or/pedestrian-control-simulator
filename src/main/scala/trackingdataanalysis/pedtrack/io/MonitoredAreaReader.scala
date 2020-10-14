package trackingdataanalysis.pedtrack.io

import hubmodel.Position
import tools.cells.Rectangle
import play.api.libs.json.{JsError, JsSuccess, JsValue, Json}
import trackingdataanalysis.pedtrack.visiosafe.{FlowLineDA}

import scala.io.BufferedSource

class MonitoredAreaReader(file: String) {

  val (monitoredArea, flowLines): (Rectangle, Vector[FlowLineDA]) = {
    val source: BufferedSource = scala.io.Source.fromFile(file)
    val input: JsValue = Json.parse(try source.mkString finally source.close)

    input.validate[MonitoredZoneCollection_JSON] match {
      case s: JsSuccess[MonitoredZoneCollection_JSON] => {
        (
          new Rectangle(s.get.ma.name, new Position(s.get.ma.x1, s.get.ma.y1), new Position(s.get.ma.x2, s.get.ma.y2), new Position(s.get.ma.x3, s.get.ma.y3), new Position(s.get.ma.x4, s.get.ma.y4), false, None),
          s.get.fl.map(data => new FlowLineDA(
            data.name, ((data.x1, data.y1), (data.x2, data.y2)),
            if (data.inflow_type == "CONTROLLED") CONTROLLED else if (data.inflow_type == "UNCONTROLLED") UNCONTROLLED else throw new IllegalArgumentException("Inflow type from JSON file is wrong ! " + data.inflow_type))
          )
        )
      }
      case e: JsError => throw new Error("Error while parsing json file: " + JsError.toJson(e).toString())
    }
  }
}
