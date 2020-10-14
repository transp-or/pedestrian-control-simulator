package trackingdataanalysis.pedtrack.visiosafe

import tools.cells.Rectangle

class FlowDensityCalculation(zoneFile: String) {

  /** Map from zone_ID to Zone. */
  val zones: Map[Int, Rectangle] = (for (l <- io.Source.fromFile(zoneFile).getLines.drop(1)) yield l.split(",").map(_.trim.toInt).head -> new Rectangle(l.split(",").map(_.trim.toDouble))).toMap


}
