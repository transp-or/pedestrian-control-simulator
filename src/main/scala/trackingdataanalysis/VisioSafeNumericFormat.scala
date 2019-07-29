package trackingdataanalysis

import hubmodel.tools.Time

final case class VisioSafeNumericFormat(f: String, headers: Option[Vector[String]]) extends TrackingDataFormat(f) {
  val separator: Char = ','
  val nbrColumns: Int = 11

}

object VisioSafeNumericFormat {

  /** function to convert VisioSafe time to seconds of one day
    *
    * @param hmsms array containing the h, m, s, ms in 3,4,5,6 positions.
    * @return number of seconds passed on that day
    */
  def time2Seconds(hmsms: Array[Double]): Time = Time(hmsms(3) * 3600.0 + hmsms(4) * 60.0 + hmsms(5) + hmsms(6) / 1000.0)

}