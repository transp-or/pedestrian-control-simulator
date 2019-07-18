package trackingdataanalysis.pedtrack.visiosafe

import hubmodel.Position
import hubmodel.tools.cells.Rectangle
import trackingdataanalysis.pedtrack.{Pedestrian, time2Seconds}

/** Container for the procssing methods common to both single day and multi day processing
  *
  * @param zoneFile location of the file specifying the zones
  */
abstract class DataProcessor(zoneFile: String, tolerance: Double) {

  /** Map from zone_ID to Zone. */
  val zones: Map[Int, Rectangle] = (for (l <- io.Source.fromFile(zoneFile).getLines.drop(1)) yield l.split(",").map(_.trim.toInt).head -> new Rectangle(l.split(",").map(_.trim.toDouble / 1000.0))).toMap
  //println(zones.values.map(z => (z.A, z.B, z.C, z.D).toString).mkString("\n"))
  /** Takes as input the file containing the data and returns the data aggregated by pedestrian. The aggregation takes
    * process is done by matching the ID to already existing processed IDs and then updates the variables.
    *
    * @param fileName location of the raw data
    * @return aggregated data
    */
  def aggregatePedestrians(fileName: String): PedestrianMap = {
    val ped: collection.mutable.Map[Int, Pedestrian] = collection.mutable.Map()
    val bufferedSource: scala.io.BufferedSource = io.Source.fromFile(fileName)
    for (l <- bufferedSource.getLines) {
      val cols = l.split(",").map(str => str.toInt)
      if (ped.contains(cols.last)) {
        ped(cols.last).updatePedestrian(cols(8).toDouble / 1000.0, cols(9).toDouble / 1000.0, time2Seconds(cols))
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
      if (pair._2.isInside(new Position(pos._1, pos._2))) pair._1
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
    * @param tol   tolerance on the times (t +- tol)
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


  /*protected def collectIDByTime(times: Seq[Double], pop: Map[Int, Vector[(Double, (Double, Double))]]): Vector[(Double, Iterable[Int])] = {
    val popMod: Iterable[(Int, Seq[Double], Seq[Double], Seq[Double])] = pop.map(p => (p._1, p._2.map(_._1), p._2.map(_._2._1), p._2.map(_._2._2)))
    collectIDByTime(times, popMod, Vector())
  }*/

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

  /** Based on the functions passed as argument, computes the metrics per time window of all the pedestrians satisfying
    * the predicate "filter".
    *
    * @param filter     predicate used to filter the population
    * @param pedFunc    extracts the metric from an individual
    * @param windowFunc computes the index of the window in which the pedestrian belongs
    * @param metricFunc computes the statistics of the metric
    * @return map where the keys are the time intervals and the values the statstics of the metric
    */
  //def aggregateMetricByTimeWindow(filter: Pedestrian => Boolean, pedFunc: Pedestrian => Double, windowFunc: Pedestrian => Int, metricFunc: Iterable[Double] => (Int, Double, Double, Double, Double, Double)): Map[Int, (Int, Double, Double, Double, Double, Double)]
}