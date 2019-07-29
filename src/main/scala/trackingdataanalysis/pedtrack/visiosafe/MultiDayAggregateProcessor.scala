package trackingdataanalysis.pedtrack.visiosafe

import java.io.{BufferedWriter, File, FileWriter}

import myscala.math.stats.ComputeStats
import myscala.output.SeqTuplesExtensions.SeqTuplesWriter
import trackingdataanalysis.pedtrack.{DataSpecification, Pedestrian, TrajectoryProcessing, ZoneProcessing}


/** Processor for multiple days of VisioSafe processing
  *
  * @param files    List of files to process
  * @param zoneFile location of the file specifying the zones
  */
class MultiDayAggregateProcessor(files: Vector[String], zoneFile: String, tolerance: Double) extends ZoneProcessing(zoneFile, tolerance) with TrajectoryProcessing {

  //private val bufferedSource: scala.io.BufferedSource = io.Source.fromFile(files.head)


  /*override val numSeq: Stream[Array[Int]] = {
    def numSeq: Stream[Array[Int]] =  bufferedSource.getLines().next.split(",").map(str => str.toInt) #:: numSeq
    numSeq
  }*/

  /** assign zones for multiple days
    *
    */
  protected def assignZones(): Unit = {
    this.ped.map(t => t._2.transform((id, pedestrian) => assignZonePedestrian(pedestrian)))
  }

  def writeDisaggregateODToCSV(path: String = ""): Unit = {
    this.ped.foreach(pd => pd._2.map(p => (p._2.UUID, p._2.entryTime, p._2.oZone, p._2.exitTime, p._2.dZone)).toVector.writeToCSV(pd._1.fileName + "_disaggOD.csv", path))
  }

  val ped: Map[DataSpecification, PedestrianMap] = files.map(str => DataSpecification(str) -> aggregatePedestrians(str)).toMap
  this.assignZones()

  /** Based on the functions passed as argument, computes the metrics per time window of all the pedestrians satisfying
    * the predicate "filter".
    *
    * @param filter     predicate used to filter the population
    * @param pedFunc    extracts the metric from an individual
    * @param windowFunc computes the index of the window in which the pedestrian belongs
    * @param metricFunc computes the statistics of the metric
    * @return map where the keys are the time intervals and the values the statstics of the metric
    */
  def aggregateMetricByTimeWindow(filter: Pedestrian => Boolean, pedFunc: Pedestrian => Double, windowFunc: Pedestrian => Int, metricFunc: Iterable[Double] => (Int, Double, Double, Double, Double, Double)): Map[DataSpecification, Map[Int, (Int, Double, Double, Double, Double, Double)]] = {
    this.ped.map(dayPop => dayPop._1 -> dayPop._2.values.filter(filter).groupBy(windowFunc).map(grouped => grouped._1 -> grouped._2.map(pedFunc).stats))
  }

  def writePedestriansToJSON(fileName: String): Unit = {

    ped.foreach(pop => {
      val file = new File(pop._1.toString + "-" + fileName)
      val bw = new BufferedWriter(new FileWriter(file))

      def helper(data: Iterable[Pedestrian]): Unit = {
        if (data.size == 1) {
          bw.write(data.head.toJSON4HubModel)
        } else {
          bw.write(data.head.toJSON4HubModel + ",\n")
          helper(data.tail)
        }
      }

      bw.write("[")
      helper(pop._2.values.toVector.sortBy(_.ID))
      bw.write("]")
      bw.close()
    })
  }


}
