package hubmodel.io.input.JSONReaders

import play.api.libs.functional.syntax._
import play.api.libs.json.Reads._
import play.api.libs.json._
import tools.Time

package object TRANSFORM {

  private[JSONReaders] case class Vehicle_JSON_TF(ID: String, stopID: String, dep: Time, arr: Time)

  implicit val Vehicle_JSON_TF_Reads: Reads[Vehicle_JSON_TF] = (
    (JsPath \ "trip_id").read[String] and
      (JsPath \ "stop_id").read[String] and
      (JsPath \ "departure_time").read[Time] and
      (JsPath \ "arrival_time").read[Time]
    ) (Vehicle_JSON_TF.apply _)

  case class Pedestrian_JSON_TF(ID: String, oZone: String, dZone: String, oTime: Option[Double])

  implicit val Pedestrian_JSON_TF_Reads: Reads[Pedestrian_JSON_TF] = (
    (JsPath \ "pass_id").read[String] and
      (JsPath \ "origin").read[String] and
      (JsPath \ "destination").read[String] and
      (JsPath \ "origin_time").readNullable[Double]
    ) (Pedestrian_JSON_TF.apply _)

  private[JSONReaders] case class PTSchedule_JSON_TF(loc: String, _timeTableInput: Vector[Vehicle_JSON_TF])

  implicit val PTSchedule_JSON_TF_Reads: Reads[PTSchedule_JSON_TF] = (
    (JsPath \ "location").read[String] and
      (JsPath \ "trains").read[Vector[Vehicle_JSON_TF]]
    ) (PTSchedule_JSON_TF.apply _)

  implicit val PublicTransportScheduleReaderTF_Reads: Reads[PublicTransportScheduleReaderTF] = (
    (JsPath \ "location").read[String](minLength[String](1)) and
      (JsPath \ "trains").read[Vector[Vehicle_JSON_TF]]
    ) (PublicTransportScheduleReaderTF.apply _)

  //case class PedestrianCollection_JSON_TF(loc: String, pop: Vector[Pedestrian_JSON_TF])

  implicit val PedestrianCollection_JSON_TF_Reads: Reads[PedestrianCollectionReaderTF] = (
    (JsPath \ "location").read[String] and
      (JsPath \ "PTFlows").read[Vector[Pedestrian_JSON_TF]]
    ) (PedestrianCollectionReaderTF.apply _)
}
