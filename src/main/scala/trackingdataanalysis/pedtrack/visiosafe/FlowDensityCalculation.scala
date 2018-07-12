package trackingdataanalysis.pedtrack.visiosafe

class FlowDensityCalculation(zoneFile: String) {

  /** Map from zone_ID to Zone. */
  val zones: Map[Int, Zone] = (for (l <- io.Source.fromFile(zoneFile).getLines.drop(1)) yield l.split(",").map(_.trim.toInt).head -> new Zone(l.split(",").map(_.trim.toDouble))).toMap


}
