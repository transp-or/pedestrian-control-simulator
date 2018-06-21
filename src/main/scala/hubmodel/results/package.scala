package hubmodel

import hubmodel.ped.{PedestrianSim}
import hubmodel.tools.cells.Rectangle
import myscala.math.stats.ComputeStats

package object results {


  implicit class PopulationProcessing(pop: Vector[PedestrianSim]) {

    /** Based on the functions passed as argument, computes the metrics per time window of all the pedestrians satisfying
      * the predicate "filter".
      *
      * @param filter     predicate used to filter the population
      * @param pedFunc    extracts the metric from an individual
      * @param windowFunc computes the index of the window in which the pedestrian belongs
      * @return map where the keys are the time intervals and the values the statstics of the metric
      */
    def aggregateMetricByTimeWindow(filter: PedestrianSim => Boolean, pedFunc: PedestrianSim => Double, windowFunc: PedestrianSim => Double): Map[Double, (Int, Double, Double, Double, Double, Double)] = {
      this.pop.filter(filter).groupBy(windowFunc).map(grouped => grouped._1 -> grouped._2.map(pedFunc).stats)
    }

    def aggregateMetricByOD(filter: PedestrianSim => Boolean, pedFunc: PedestrianSim => Double): Map[(Rectangle, Rectangle), (Int, Double, Double, Double, Double, Double)] = {
      this.pop.filter(filter).groupBy(p => (p.origin, p.finalDestination)).map(grouped => grouped._1 -> grouped._2.map(pedFunc).stats)
    }

    //.writeToCSV(config.getString("output.output_prefix") + "_travel_times_OD_stats.csv", columnNames = Some(Vector("O", "D", "size", "mean", "variance", "median", "min", "max")), rowNames = None)

  }

}
