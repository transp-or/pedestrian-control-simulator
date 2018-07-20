package hubmodel.demand
/*
@deprecated
class ReadSchedule(fileName: String) {

  val source: BufferedSource = scala.io.Source.fromFile(fileName)
  val input: JsValue = Json.parse(try source.mkString finally source.close)

  input.validate[PublicTransportScheduleReader] match {
    case s: JsSuccess[PublicTransportScheduleReader] => new PublicTransportSchedule(s.get.loc, s.get._timeTableInput.map(v => new Vehicle(v.ID, v.trainType, v.track, v.arr, v.dep, v.capacity)))
    case e: JsError => throw new Error("Error while parsing train timetable: " + JsError.toJson(e).toString())
  }

}
*/