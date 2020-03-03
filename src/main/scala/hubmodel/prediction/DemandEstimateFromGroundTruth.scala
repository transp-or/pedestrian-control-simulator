package hubmodel.prediction

import hubmodel.control.amw.MovingWalkway
import hubmodel.ped.PedestrianNOMAD
import tools.Time
import tools.TimeNumeric.mkOrderingOps
import tools.cells.Vertex


class DemandEstimateFromGroundTruth(population: Vector[PedestrianNOMAD], movingWalkways: Vector[MovingWalkway], intervals: Vector[Time]) extends DemandEstimate {

  private val amwAlternativeVerticesFilter: Vector[Vertex] = this.movingWalkways.flatMap(amw => amw.parallelFlows.flatten ++ Vector(amw.startVertex, amw.endVertex))
  private val parallelFlows: Vector[(String, Vector[Vertex], Int)] = this.movingWalkways.flatMap(amw => amw.parallelFlows.flatMap(pf => Vector((amw.ID, Vector(amw.startVertex, amw.endVertex), 1),(amw.ID, pf, 1), (amw.ID, pf.reverse, -1))))

  //val test: Iterable[T] = population.filter(ped => ped.accomplishedRoute.exists(tv => intervals.head <= tv._1 && tv._1 <= intervals.last && amwAlternativeVerticesFilter.contains(tv._2)))

  private val flowsByAMW: Iterable[(String, Int, Vector[(Time, Vertex, Int)])] = population.collect {
    case ped if ped.accomplishedRoute.exists(tv => intervals.head <= tv._1 && tv._1 <= intervals.last && amwAlternativeVerticesFilter.contains(tv._2)) => {

      parallelFlows.collect{case pf if ped.accomplishedRoute.map(_._2).containsSlice(pf._2) => {
        val idx: Int = ped.accomplishedRoute.map(_._2).indexOfSlice(pf._2)
        (pf._1, pf._3, ped.accomplishedRoute.slice(idx, idx + pf._2.length).map(tv => (tv._1, tv._2, intervals.indexWhere(_ > tv._1))))
      }}
    }
  }.flatten


  val flows: Map[(String, Int, Int), Int] = flowsByAMW.groupBy(g => (g._1, g._2, g._3.head._3)).view.mapValues(_.size).toMap

}
