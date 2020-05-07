package trackingdataanalysis.pedtrack

import hubmodel.Position
import tools.cells.Rectangle
import trackingdataanalysis.pedtrack.visiosafe.PedestrianMap
import trackingdataanalysis.{SBBFormat, TrackingDataFormat, VisioSafeNumericFormat}

/** Container for the procssing methods common to both single day and multi day processing
  *
  * @param zoneFile location of the file specifying the zones
  */
abstract class ZoneProcessing(zoneFile: String, tolerance: Double) {

  /** Map from zone_ID to Zone. */
  val zones: Map[Int, Rectangle] = (for (l <- scala.io.Source.fromFile(zoneFile).getLines.drop(1)) yield l.split(",").map(_.trim.toInt).head -> new Rectangle(l.split(",").map(_.trim.toDouble / 1000.0))).toMap

  /** Takes as input the file containing the data and returns the data aggregated by pedestrian. The aggregation takes
    * process is done by matching the ID to already existing processed IDs and then updates the variables.
    *
    * @param fileName location of the raw data
    * @return aggregated data
    */
  @deprecated
  def aggregatePedestrians(fileName: String): PedestrianMap = {
    val ped: PedestrianMap = collection.mutable.Map()
    val bufferedSource: scala.io.BufferedSource = scala.io.Source.fromFile(fileName)
    for (l <- bufferedSource.getLines) {
      val cols = l.split(",").map(str => str.toInt)
      if (ped.contains(cols.last)) {
        ped(cols.last).updatePedestrian(cols(8).toDouble / 1000.0, cols(9).toDouble / 1000.0, VisioSafeNumericFormat.time2Seconds(cols.map(_.toDouble)).value.toDouble)
      }
      else ped += (cols.last -> new Pedestrian(cols.last, (cols(8).toDouble / 1000.0, cols(9).toDouble / 1000.0), time2Seconds(cols)))
    }
    bufferedSource.close()
    ped
  }



  /** Returns the zone in which a point is located
    *
    * @param pos      point to find owernship
    * @param mapZones Map storing the zones
    * @return Int naming the zone, -1 if none found
    */
  private def findZoneOwnership(pos: (Double, Double), mapZones: Map[Int, Rectangle]): Int = {
    if (mapZones.isEmpty) -1
    else {
      val pair = mapZones.head
      if (pair._2.isInside(new Position(pos._1, pos._2), false)) pair._1
      else findZoneOwnership(pos, mapZones.tail)
    }
  }

  /** Returns a copy of a pedestrian with the zone, travel time and travel distance set.
    *
    * @param p pedestrian before the zone is set
    * @return the same pedestrian with updated info
    */
  protected def assignZonePedestrian(p: Pedestrian): Pedestrian = {
    p.oZone = findZoneOwnership(p.oCoords, this.zones)
    p.dZone = findZoneOwnership(p.dCoords, this.zones)
    p.travelTime = p.exitTime - p.entryTime
    p.meanVelocity = p.travelDistance / p.travelTime
    p
  }

  /** Abstract assign zones method. Must be implemented for processors.
    *
    */
  protected def assignZones(): Unit

  /** Takes the simplified version of the population stored in an [[Iterable]] and aggregates the data based on a
    * collection of times. For each time provided as input, the IDs of the pedestrians which are within a specified
    * tolerance.
    *
    * @param times collection of times for which to collect the pedestrian IDs
    * @param pop   pedestrian population
    * @param acc   accumulator
    * @return map with the times as keys and the IDs of pedestrians as values
    */
  protected def collectIDByTime(times: Seq[Double], pop: Iterable[(Int, Seq[Double], Seq[Double], Seq[Double])], acc: Vector[(Double, Iterable[Int])]): Vector[(Double, Iterable[Int])] = {
    if (times.isEmpty) acc
    else {
      collectIDByTime(
        times.tail,
        pop,
        acc :+ (times.head -> pop.filter(p => p._2.indexWhere(t => times.head - tolerance <= t && t < times.head + tolerance) >= 0).map(_._1).toVector.distinct)
      )
    }
  }

  /** Linear interpolation between points. This is usefull when the timestamps aren't at regular intervals.
    *
    * @param x1 first point
    * @param x2 second point
    * @param t1 first time
    * @param t2 second time
    * @param tDesired desired time
    * @return point at the desired time using a linear interpolation
    */
  def linearInterpolationPosition(x1: (Double, Double), x2: (Double, Double), t1: Double, t2: Double, tDesired: Double): (Double, Double) = {
    if (t1 > t2) {
      throw new IllegalArgumentException("t1 is larger than t2 ! t1=" + t1 + ", t2=" + t2)
    }
    else if (tDesired < t1 || t2 < tDesired) {
      throw new IllegalArgumentException("desired time is outside of interval for linear interpolation: " + t1 + ", " + t2 + ", " + tDesired)
    }
    val frac: Double = (tDesired - t1) / (t2 - t1)
    (x1._1 + frac * (x2._1 - x1._1), x1._2 + frac * (x2._2 - x1._2))
  }

  /** Returns the index of the inteval in which the argument lies
    *
    * @param t     value wanted to be placed
    * @param times vector in which to place the argument
    * @return index of the correct position
    */
  def findInterval(t: Double, times: Vector[Double]): Int = {
    times.indexWhere(_ > t)
  }
}