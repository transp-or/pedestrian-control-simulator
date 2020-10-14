package hubmodel.prediction

import hubmodel.Position
import hubmodel.control.amw.{MovingWalkway, MovingWalkwayAbstract}
import hubmodel.ped.History.HistoryContainer
import hubmodel.ped.{Pedestrian, PedestrianNOMAD, PedestrianSim, PedestrianTrait, PedestrianTrajectory}
import myscala.math.vector.Vector2D
import myscala.math.stats.computeQuantile
import tools.Time
import tools.TimeNumeric.mkOrderingOps
import tools.cells.{DensityMeasuredArea, Vertex}


trait AMWFlowsHelpers {

  protected val intervals: Vector[Time]

  protected val amwAlternativeVerticesFilter: Vector[Vertex]

  protected val parallelFlows: Vector[(String, Vector[Vertex], Vector2D, Int, Double)]

  //val aggregateFlowsByAMW: Map[(String, Int, Int), Double]/* = flowsByAMW(population).groupBy(g => (g._1, g._2, g._3)).view.mapValues(kv => kv.map(_._4).sum).toMap*/

  protected def flowsByAMW(pop: Vector[PedestrianTrajectory]): Iterable[(String, Int, Int, Double)] = {
    pop.filter(p => {p.accomplishedRoute.map(_._2).intersect(amwAlternativeVerticesFilter).nonEmpty})
      .flatMap(p => {
        parallelFlows.flatMap(pf => {
          val idx: Int = p.accomplishedRoute.map(_._2).indexOfSlice(pf._2)
          p match {
            case ped if idx >= 0 => {
              val pedSubRoute: Vector[(Time, Vertex, Position)] = ped.accomplishedRoute.slice(idx, idx + pf._2.size)

              intervals.sliding(2).zipWithIndex.map(a => (a._1.head, a._1.last, a._2)).collect {
                // pedestrian entered link before start of interval and after the end during interval
                case a if a._1 >= pedSubRoute.head._1 && pedSubRoute.last._1 > a._2 && pedSubRoute.last._1 > a._2 => {

                  val posAtEntry: Position = ped.getHistoryPosition.minBy(v => math.abs((v._1 - a._1).value.toDouble))._2.pos
                  val posAtExit: Position = ped.getHistoryPosition.minBy(v => math.abs((v._1 - a._2).value.toDouble))._2.pos

                  //println( ((pf._3 * (posAtExit - pf._2._1.center).dot(pf._3)).norm - (pf._3 * (posAtEntry - pf._2._1.center).dot(pf._3)).norm) / (pf._5 * (a._2 - a._1).value.toDouble ) )

                  (pf._1, pf._4, a._3, ((pf._3 * (posAtExit - pf._2.head.center).dot(pf._3)).norm - (pf._3 * (posAtEntry - pf._2.head.center).dot(pf._3)).norm) / (pf._5 * (a._2 - a._1).value.toDouble ) )
                }
                // pedestrian entered link before start of interval and left link before the end of interval
                case b if b._1 >= pedSubRoute.head._1 && pedSubRoute.last._1 > b._1 && pedSubRoute.last._1 < b._2 => {

                  val posAtEntry: Position = ped.getHistoryPosition.minBy(v => math.abs((v._1 - b._1).value.toDouble))._2.pos

                  //println( (posAtEntry - pf._2._1.center).dot(pf._3) )
                  //println(  pf._3 * (posAtEntry - pf._2._1.center).dot(pf._3) )
                  //println( (pf._5 - (pf._3 * (posAtEntry - pf._2._1.center).dot(pf._3)).norm) / (pf._5 * (b._2 - b._1).value.toDouble ))

                  (pf._1, pf._4, b._3, (pf._5 - (pf._3 * (posAtEntry - pf._2.head.center).dot(pf._3)).norm) / (pf._5 * (b._2 - b._1).value.toDouble ) )
                }
                // pedestrian entered link after start of interval and left link before the end of interval (pedestrian was inside for whole interval)
                case c if c._1 <= pedSubRoute.head._1 && pedSubRoute.head._1 < c._2 && pedSubRoute.last._1 <= c._2 => {
                  //println(1.0 / (c._2 - c._1).value.toDouble)
                  (pf._1, pf._4, c._3, 1.0 / (c._2 - c._1).value.toDouble)
                }
                // pedestrian entered link after start of interval and left link after the end during interval
                case d if d._1 <= pedSubRoute.head._1 && pedSubRoute.head._1 < d._2 && pedSubRoute.last._1 >= d._2 => {

                  val posAtEnd: Position = ped.getHistoryPosition.minBy(v => math.abs((v._1 - d._2).value.toDouble))._2.pos

                  //println( (pf._3 * (posAtEnd - pf._2._1.center).dot(pf._3)).norm / (pf._5 * (d._2 - d._1).value.toDouble ) )

                  (pf._1, pf._4, d._3, (pf._3 * (posAtEnd - pf._2.head.center).dot(pf._3)).norm / (pf._5 * (d._2 - d._1).value.toDouble ) )
                }
              }
            }
            case ped: PedestrianSim if idx < 0 && ped.accomplishedRoute.last._2 == pf._2.head && ped.nextZone == pf._2.last => {
              val pedEnteringTime: Time = ped.accomplishedRoute.last._1
              intervals.sliding(2).zipWithIndex.map(a => (a._1.head, a._1.last, a._2)).collect{

                // Pedestrian entered before start of interval and is still inside
                case a if a._1 <= pedEnteringTime && pedEnteringTime < a._2 => {

                  //println((pf._3 * (ped.currentPosition - pf._2._1.center).dot(pf._3)).norm / (pf._5 * (a._2 - a._1).value.toDouble ))
                  (pf._1, pf._4, a._3, (pf._3 * (ped.currentPosition - pf._2.head.center).dot(pf._3)).norm / (pf._5 * (a._2 - a._1).value.toDouble ) )

                }
                // Pedestrian entered during interval and is still inside at end of interval
                case b if pedEnteringTime < b._1 => {
                  val posAtEntry: Position = ped.getHistoryPosition.minBy(v => math.abs((v._1 - b._1).value.toDouble))._2.pos
                  val posAtExit: Position = ped.getHistoryPosition.minBy(v => math.abs((v._1 - b._2).value.toDouble))._2.pos

                  //println( ((pf._3 * (posAtExit - pf._2._1.center).dot(pf._3)).norm - (pf._3 * (posAtEntry - pf._2._1.center).dot(pf._3)).norm) / (pf._5 * (b._2 - b._1).value.toDouble ) )

                  (pf._1, pf._4, b._3, ((pf._3 * (posAtExit - pf._2.head.center).dot(pf._3)).norm - (pf._3 * (posAtEntry - pf._2.head.center).dot(pf._3)).norm) / (pf._5 * (b._2 - b._1).value.toDouble ) )

                }
              }
            }
            case _ => {Vector()}
          }
        })
      })
  }
}

class AMWFlowsFromEmpiricalData(pop: Vector[PedestrianTrajectory], val intervals: Vector[Time], amwRoutes: Map[String, Vector[Vector[Vertex]]]) extends DemandEstimate with AMWFlowsHelpers {

  protected val amwAlternativeVerticesFilter: Vector[Vertex] = this.amwRoutes.values.flatten.flatten.toVector

  protected val parallelFlows: Vector[(String, Vector[Vertex], Vector2D, Int, Double)] = this.amwRoutes.toVector.flatMap(r => r._2.flatMap(pf => Vector(
    (r._1, pf, (pf.head.center - pf.last.center).normalized, 1, (pf.head.center - pf.last.center).norm),
    (r._1, pf.reverse, (pf.last.center - pf.head.center).normalized, -1, (pf.last.center - pf.head.center).norm)
  )))

  val aggregateFlowsByAMW: Map[(String, Int, Int), Double] = flowsByAMW(pop).groupBy(g => (g._1, g._2, g._3)).view.mapValues(kv => kv.map(_._4).sum).toMap

}


class AMWFlowsFromGroundTruthProcessor(movingWalkways: Vector[MovingWalkwayAbstract], val intervals: Vector[Time]) extends DemandEstimate with AMWFlowsHelpers {

  protected val amwAlternativeVerticesFilter: Vector[Vertex] = this.movingWalkways
    .flatMap(amw => amw.parallelFlows.flatten ++ Vector(amw.firstVertex, amw.secondVertex))

  protected val parallelFlows: Vector[(String, Vector[Vertex], Vector2D, Int, Double)] = this.movingWalkways
    .flatMap(amw => amw.parallelFlows.flatMap(pf => {Vector(
      (amw.name, Vector(amw.firstVertex, amw.secondVertex), (amw.secondVertex.center - amw.firstVertex.center).normalized, 1, (amw.secondVertex.center - amw.firstVertex.center).norm),
      (amw.name, Vector(amw.secondVertex, amw.firstVertex), (amw.firstVertex.center - amw.secondVertex.center).normalized, -1, (amw.firstVertex.center - amw.secondVertex.center).norm),
      (amw.name, pf, (pf.last.center - pf.head.center).normalized, 1, (pf.last.center - pf.head.center).norm),
      (amw.name, pf.reverse, (pf.head.center - pf.last.center).normalized, -1, (pf.head.center - pf.last.center).norm)
    )}))

  //val aggregateFlowsByAMW: Map[(String, Int, Int), Double] = flowsByAMW(population).groupBy(g => (g._1, g._2, g._3)).view.mapValues(kv => kv.map(_._4).sum).toMap

  def aggregateFlowsByAMW(pop: Vector[PedestrianSim]): AMWFlowsFromGroundTruth = new AMWFlowsFromGroundTruth(flowsByAMW(pop).groupBy(g => (g._1, g._2, g._3)).view.mapValues(kv => kv.map(_._4).sum).toMap)
}

class AMWFlowsFromGroundTruth(val aggregateFlowsByAMW: Map[(String, Int, Int), Double])

class CongestionDataFromGroundTruth(criticalAreas: Map[String, DensityMeasuredArea], amws: Vector[MovingWalkway], intervals: Vector[Time]) extends DemandEstimate {

  val quantile75DensityByArea: Map[(String, Int), Double] = criticalAreas.flatMap(area =>
    area._2.paxIndividualDensityHistory.toVector
      .map(v => (area._1, intervals.sliding(2).indexWhere(i => i.head.value <= v._1.value.toDouble && v._1.value.toDouble < i.last.value)) -> {
        if (v._2.isEmpty) {
          0.0
        } else {
          computeQuantile(75)(v._2).value
        }
      })
  )


  val amwsZones: Map[String, (Vector[String], Vector[String])] = amws.map(w => w.name -> (w.criticalAreaStart.map(_.name).toVector, w.criticalAreaEnd.map(_.name).toVector)).toMap
}
