package hubmodel.prediction

import hubmodel.control.amw.MovingWalkway
import hubmodel.ped.PedestrianNOMAD
import tools.Time
import tools.TimeNumeric.mkOrderingOps
import tools.cells.{DensityMeasuredArea, Vertex}


class AMWFlowsFromGroundTruth(population: Vector[PedestrianNOMAD], movingWalkways: Vector[MovingWalkway], intervals: Vector[Time]) extends DemandEstimate {

  private val amwAlternativeVerticesFilter: Vector[Vertex] = this.movingWalkways
    .flatMap(amw => amw.parallelFlows.flatten ++ Vector(amw.startVertex, amw.endVertex))

  private val parallelFlows: Vector[(String, Vector[Vertex], Int)] = this.movingWalkways
    .flatMap(amw => amw.parallelFlows.flatMap(pf => Vector( (amw.name, Vector(amw.startVertex, amw.endVertex), 1), (amw.name, Vector(amw.endVertex, amw.startVertex), -1),  (amw.name, pf, 1), (amw.name, pf.reverse, -1))))


  private val flowsByAMW: Iterable[(String, Int, Int, Double)] = population
    .filter(p => {
      p.accomplishedRoute.map(_._2).intersect(amwAlternativeVerticesFilter).nonEmpty
    })
    .flatMap(p => {
      parallelFlows.flatMap(pf => {
        val idx: Int = p.accomplishedRoute.map(_._2).indexOfSlice(pf._2)
        if (idx >= 0) {
          val pedSubRoute: Vector[(Time, Vertex)] = p.accomplishedRoute.slice(idx, idx + pf._2.size)

          intervals.dropRight(1).zip(intervals.tail).zipWithIndex.map(a => (a._1._1, a._1._2, a._2)).collect {
            // pedestrian entered during interval and is still inside at end of interval
            case a if a._1 <= pedSubRoute.head._1 && pedSubRoute.head._1 < a._2 && pedSubRoute.last._1 > a._2 => {
              (pf._1, pf._3, a._3, ((a._2 - pedSubRoute.head._1).value / (a._2 - a._1).value).toDouble)
            }
            // pedestrian entered during interval and exited during interval
            case b if b._1 <= pedSubRoute.head._1 && pedSubRoute.head._1 < b._2 && pedSubRoute.last._1 < b._2 => {
              (pf._1, pf._3, b._3, ((pedSubRoute.last._1 - pedSubRoute.head._1).value / (b._2 - b._1).value).toDouble)
            }
            // pedestrian entered before start of interval and left after end of interval (pedestrian was inside for whole interval)
            case c if c._1 >= pedSubRoute.head._1 && pedSubRoute.last._1 >= c._2 => {
              (pf._1, pf._3, c._3, 1.0)
            }
            // pedestrian entered before start of interval and left during interval
            case d if d._1 >= pedSubRoute.head._1 && pedSubRoute.last._1 <= d._2 && pedSubRoute.last._1 >= d._1 => {
              (pf._1, pf._3, d._3, ((pedSubRoute.last._1 - d._1).value / (d._2 - d._1).value).toDouble)
            }
          }

        } else if (p.accomplishedRoute.last._2 == pf._2.head && p.nextZone == pf._2.last) {

          val pedEnteringTime: Time = p.accomplishedRoute.last._1
          intervals.sliding(2).zipWithIndex.map(a => (a._1.head, a._1.last, a._2)).collect{
                // pedestrian entered during interval and is still inside at end of interval
            case a if a._1 <= pedEnteringTime && pedEnteringTime < a._2 => {
              (pf._1, pf._3, a._3, ((a._2 - pedEnteringTime).value/(a._2 - a._1).value).toDouble)
            }
              // Pedestrian already inside link at beginning of interval and hasn't left yet
            case b if pedEnteringTime < b._1 => {
              (pf._1, pf._3, b._3, 1.0)
            }
          }
        } else {
          Vector()
        }
      })
    })

  val aggregateFlowsByAMW: Map[(String, Int, Int), Double] = flowsByAMW.groupBy(g => (g._1, g._2, g._3)).view.mapValues(kv => kv.map(_._4).sum).toMap
}

class CongestionDataFromGroundTruth(criticalAreas: Map[String, DensityMeasuredArea], intervals: Vector[Time]) extends DemandEstimate {

  val densityThreshold: Double = 3.0

  val individualDensitiesByArea: Map[(String, Int), Vector[Double]] = criticalAreas.flatMap(area => {
    val densitiesPerTimeInterval = area._2.paxIndividualDensityHistory
      .toVector
      .groupBy(v => intervals.indexWhere(_ > v._1))
      .view.mapValues(_.flatMap(_._2))

    densitiesPerTimeInterval.map(dv => (area._1, dv._1) -> dv._2)
  })

  /*individualDensitiesByArea.filter(area => computeQuantile(75)(area._2).value > densityThreshold)
  movingWalkways.map(w => criticalAreas(w.startArea))
*/
}
