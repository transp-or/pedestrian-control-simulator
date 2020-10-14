package trackingdataanalysis.pedtrack

import play.api.libs.functional.syntax._
import play.api.libs.json._

package object io {

  case class Zone_JSON(name: String, x1: Double, y1: Double, x2: Double, y2: Double, x3: Double, y3: Double, x4: Double, y4: Double)

  implicit val Cell_JSONWrites: Writes[Zone_JSON] = (
    (JsPath \ "id").write[String] and
      (JsPath \ "x1").write[Double] and
      (JsPath \ "y1").write[Double] and
      (JsPath \ "x2").write[Double] and
      (JsPath \ "y2").write[Double] and
      (JsPath \ "x3").write[Double] and
      (JsPath \ "y3").write[Double] and
      (JsPath \ "x4").write[Double] and
      (JsPath \ "y4").write[Double]
    ) (unlift(Zone_JSON.unapply))

  /** Wrtier for SF infrastructure specifications
    *
    */
  implicit val Cell_JSONReads: Reads[Zone_JSON] = (
    (JsPath \ "id").read[String] and
      (JsPath \ "x1").read[Double] and
      (JsPath \ "y1").read[Double] and
      (JsPath \ "x2").read[Double] and
      (JsPath \ "y2").read[Double] and
      (JsPath \ "x3").read[Double] and
      (JsPath \ "y3").read[Double] and
      (JsPath \ "x4").read[Double] and
      (JsPath \ "y4").read[Double]
    ) (Zone_JSON.apply _)

  type InflowType = Int
  final val CONTROLLED: InflowType = 1
  final val UNCONTROLLED: InflowType = 0

  case class FlowLine_JSON(name: String, x1: Double, y1: Double, x2: Double, y2: Double, inflow_type: String)

  implicit val FlowLine_JSONWrites: Writes[FlowLine_JSON] = (
    (JsPath \ "id").write[String] and
      (JsPath \ "x1").write[Double] and
      (JsPath \ "y1").write[Double] and
      (JsPath \ "x2").write[Double] and
      (JsPath \ "y2").write[Double] and
      (JsPath \ "inflow_type").write[String]
    ) (unlift(FlowLine_JSON.unapply))

  /** Wrtier for SF infrastructure specifications
    *
    */
  implicit val FlowLine_JSONReads: Reads[FlowLine_JSON] = (
    (JsPath \ "id").read[String] and
      (JsPath \ "x1").read[Double] and
      (JsPath \ "y1").read[Double] and
      (JsPath \ "x2").read[Double] and
      (JsPath \ "y2").read[Double] and
      (JsPath \ "inflow_type").read[String]
    ) (FlowLine_JSON.apply _)

  case class MonitoredZoneCollection_JSON(ma: Zone_JSON, fl: Vector[FlowLine_JSON])

  implicit val FlowLineCollectionWrites: Writes[MonitoredZoneCollection_JSON] = (
    (JsPath).write[Zone_JSON] and
      (JsPath \ "flow_segments").write[Vector[FlowLine_JSON]]
    ) (unlift(MonitoredZoneCollection_JSON.unapply))

  /** Writer for SF infrastructure specifications
    *
    */
  implicit val FlowLineCollectionReads: Reads[MonitoredZoneCollection_JSON] = (
    (JsPath).read[Zone_JSON] and
      (JsPath \ "flow_segments").read[Vector[FlowLine_JSON]]
    ) (MonitoredZoneCollection_JSON.apply _)

}
