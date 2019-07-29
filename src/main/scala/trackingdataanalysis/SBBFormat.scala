package trackingdataanalysis

import hubmodel.tools.Time

final case class SBBFormat(f: String, headers: Option[Vector[String]]) extends TrackingDataFormat(f) {

  val separator: Char = ','
  val nbrColumns: Int = 6


}

object SBBFormat {
  def timeConverter(time: Double): Time = {
    Time(time / 1000.0)
  }
}
