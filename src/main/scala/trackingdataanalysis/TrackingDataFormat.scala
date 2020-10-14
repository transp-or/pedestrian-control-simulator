package trackingdataanalysis

abstract class TrackingDataFormat(val file: String) {
  val headers: Option[Vector[String]]
}
