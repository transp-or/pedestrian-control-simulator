package hubmodel.prediction

import hubmodel.Position
import hubmodel.control.amw.MovingWalkway
import hubmodel.ped.History.HistoryContainer
import hubmodel.ped.PedestrianNOMAD
import myscala.math.vector.Vector2D
import tools.Time
import tools.TimeNumeric.mkOrderingOps
import tools.cells.{DensityMeasuredArea, Vertex}


class AMWFlowsFromGroundTruth(population: Vector[PedestrianNOMAD], movingWalkways: Vector[MovingWalkway], intervals: Vector[Time]) extends DemandEstimate {

  private val amwAlternativeVerticesFilter: Vector[Vertex] = this.movingWalkways
    .flatMap(amw => amw.parallelFlows.flatten ++ Vector(amw.startVertex, amw.endVertex))

  private val parallelFlows: Vector[(String, (Vertex, Vertex), Vector2D, Int, Double)] = this.movingWalkways
    .flatMap(amw => amw.parallelFlows.flatMap(pf => {Vector(
      (amw.name, (amw.startVertex, amw.endVertex), (amw.endVertex.center - amw.startVertex.center).normalized, 1, (amw.endVertex.center - amw.startVertex.center).norm),
      (amw.name, (amw.endVertex, amw.startVertex), (amw.startVertex.center - amw.endVertex.center).normalized, -1, (amw.startVertex.center - amw.endVertex.center).norm),
      (amw.name, (pf.head, pf.last), (pf.last.center - pf.head.center).normalized, 1, (pf.last.center - pf.head.center).norm),
      (amw.name, (pf.last, pf.head), (pf.head.center - pf.last.center).normalized, -1, (pf.head.center - pf.last.center).norm)
    )}))

  private val flowsByAMW: Iterable[(String, Int, Int, Double)] = population
    .filter(p => {p.accomplishedRoute.map(_._2).intersect(amwAlternativeVerticesFilter).nonEmpty})
    .flatMap(p => {
      parallelFlows.flatMap(pf => {
        val idx: Int = p.accomplishedRoute.map(_._2).indexOfSlice(Vector(pf._2._1, pf._2._2))
        if (idx >= 0) {
          val pedSubRoute: Vector[(Time, Vertex, Position)] = p.accomplishedRoute.slice(idx, idx + 2)

          intervals.sliding(2).zipWithIndex.map(a => (a._1.head, a._1.last, a._2)).collect {
            // pedestrian entered link before start of interval and after the end during interval
            case a if a._1 >= pedSubRoute.head._1 && pedSubRoute.last._1 > a._2 && pedSubRoute.last._1 > a._2=> {

              val posAtEntry: Position = p.getHistoryPosition.minBy(v => math.abs((v._1 - a._1).value.toDouble))._2.pos
              val posAtExit: Position = p.getHistoryPosition.minBy(v => math.abs((v._1 - a._2).value.toDouble))._2.pos

              //println( ((pf._3 * (posAtExit - pf._2._1.center).dot(pf._3)).norm - (pf._3 * (posAtEntry - pf._2._1.center).dot(pf._3)).norm) / (pf._5 * (a._2 - a._1).value.toDouble ) )

              (pf._1, pf._4, a._3, ((pf._3 * (posAtExit - pf._2._1.center).dot(pf._3)).norm - (pf._3 * (posAtEntry - pf._2._1.center).dot(pf._3)).norm) / (pf._5 * (a._2 - a._1).value.toDouble ) )
            }
            // pedestrian entered link before start of interval and left link before the end of interval
            case b if b._1 >= pedSubRoute.head._1 && pedSubRoute.last._1 > b._1 && pedSubRoute.last._1 < b._2 => {

              val posAtEntry: Position = p.getHistoryPosition.minBy(v => math.abs((v._1 - b._1).value.toDouble))._2.pos

              //println( (posAtEntry - pf._2._1.center).dot(pf._3) )
              //println(  pf._3 * (posAtEntry - pf._2._1.center).dot(pf._3) )
              //println( (pf._5 - (pf._3 * (posAtEntry - pf._2._1.center).dot(pf._3)).norm) / (pf._5 * (b._2 - b._1).value.toDouble ))

              (pf._1, pf._4, b._3, (pf._5 - (pf._3 * (posAtEntry - pf._2._1.center).dot(pf._3)).norm) / (pf._5 * (b._2 - b._1).value.toDouble ) )
            }
            // pedestrian entered link after start of interval and left link before the end of interval (pedestrian was inside for whole interval)
            case c if c._1 <= pedSubRoute.head._1 && pedSubRoute.head._1 < c._2 && pedSubRoute.last._1 <= c._2 => {
              //println(1.0 / (c._2 - c._1).value.toDouble)
              (pf._1, pf._4, c._3, 1.0 / (c._2 - c._1).value.toDouble)
            }
            // pedestrian entered link after start of interval and left link after the end during interval
            case d if d._1 <= pedSubRoute.head._1 && pedSubRoute.head._1 < d._2 && pedSubRoute.last._1 >= d._2 => {

              val posAtEnd: Position = p.getHistoryPosition.minBy(v => math.abs((v._1 - d._2).value.toDouble))._2.pos

              //println( (pf._3 * (posAtEnd - pf._2._1.center).dot(pf._3)).norm / (pf._5 * (d._2 - d._1).value.toDouble ) )

              (pf._1, pf._4, d._3, (pf._3 * (posAtEnd - pf._2._1.center).dot(pf._3)).norm / (pf._5 * (d._2 - d._1).value.toDouble ) )
            }
          }

        } else if (p.accomplishedRoute.last._2 == pf._2._1 && p.nextZone == pf._2._2) {

          val pedEnteringTime: Time = p.accomplishedRoute.last._1
          intervals.sliding(2).zipWithIndex.map(a => (a._1.head, a._1.last, a._2)).collect{

                // Pedestrian entered before start of interval and is still inside
            case a if a._1 <= pedEnteringTime && pedEnteringTime < a._2 => {

              //println((pf._3 * (p.currentPosition - pf._2._1.center).dot(pf._3)).norm / (pf._5 * (a._2 - a._1).value.toDouble ))
              (pf._1, pf._4, a._3, (pf._3 * (p.currentPosition - pf._2._1.center).dot(pf._3)).norm / (pf._5 * (a._2 - a._1).value.toDouble ) )

            }
              // Pedestrian entered during interval and is still inside at end of interval
            case b if pedEnteringTime < b._1 => {
              val posAtEntry: Position = p.getHistoryPosition.minBy(v => math.abs((v._1 - b._1).value.toDouble))._2.pos
              val posAtExit: Position = p.getHistoryPosition.minBy(v => math.abs((v._1 - b._2).value.toDouble))._2.pos

              //println( ((pf._3 * (posAtExit - pf._2._1.center).dot(pf._3)).norm - (pf._3 * (posAtEntry - pf._2._1.center).dot(pf._3)).norm) / (pf._5 * (b._2 - b._1).value.toDouble ) )

              (pf._1, pf._4, b._3, ((pf._3 * (posAtExit - pf._2._1.center).dot(pf._3)).norm - (pf._3 * (posAtEntry - pf._2._1.center).dot(pf._3)).norm) / (pf._5 * (b._2 - b._1).value.toDouble ) )

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
