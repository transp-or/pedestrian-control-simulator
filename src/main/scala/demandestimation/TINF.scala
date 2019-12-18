package demandestimation

import com.typesafe.config.Config
import demandestimation.network.NetworkLausanne
import hubmodel.DES.getPTSchedule
import hubmodel.demand.PublicTransportSchedule
import hubmodel.demand.transit.{Train, TrainWithExchangeVolumes}
import hubmodel.supply.TrainID_New
import hubmodel.supply.graph.Stop2Vertex
import tools.math.integration.simpsonIntegration


class TINF(parameters: DemandEstimationParameters, config: Config, network: NetworkLausanne) {


  private def csvToMap(file: String): Map[String, Vector[Double]] = {
    val bufferedSource = io.Source.fromFile(file)
    val header = bufferedSource.getLines().iterator.next()
    val colsToLabels: Map[Int, String] = header.split(",").zipWithIndex.map(d => d._2 -> d._1).toMap
    val ASEData: Map[String, collection.mutable.ArrayBuffer[Double]] = header.split(",").map(h => h -> collection.mutable.ArrayBuffer[Double]()).toMap
    for (line <- bufferedSource.getLines) {
      val cols = line.split(",").map(_.trim)
      // do whatever you want with the columns here
      cols.zipWithIndex.map(c => ASEData(colsToLabels(c._2)).append(c._1.toDouble))
      //println(s"${cols(0)}|${cols(1)}|${cols(2)}|${cols(3)}")
    }
    bufferedSource.close
    ASEData.view.mapValues(_.toVector).toMap
  }


  /**   Aggregates the flows into the time intervals specified in the mian class
    *
    * @param self
    * @param tint_dict
    * @param trainObject
    * @return
    */
  def unloadingLinkCountEstimate(tint_dicts: Vector[Double], trainParameters: TrainParameters): Map[String, Vector[Double]] = {


    //println(trainParameters)

    // add one time interval to ned of intervals !

  val timeIndices = tint_dicts.dropRight(1).indices

  //linkCounts = pd.DataFrame(index=indexNames, columns=trainObject.ratios.index.values)

  //tmDateTime = datetime.datetime.strptime(tint_dictComplete[0][0:12] + str(trainObject.arrTime), "%d-%b-%Y %H:%M:%S")

    val tinf: Vector[(Int, Iterable[(String, Double)])] = (for ( i <- timeIndices) yield {
      val totalPed = simpsonIntegration(
        PWL(trainParameters.trainArrival - tint_dicts.head, trainParameters.deadTime, trainParameters.Q, trainParameters.alpha),
        tint_dicts(i) - tint_dicts.head,
        tint_dicts(i+1) - tint_dicts.head,
        500)
      (i, trainParameters.ratios.map(r => (r._1, r._2 * totalPed)))
    }).toVector

    //println(trainParameters, tinf.mkString("\n"))

    Map() ++ tinf.flatMap(_._2.map(_._1)).distinct.map(sec => sec -> tinf.flatMap(_._2.filter(_._1 == sec).map(_._2)))
}


  def PWL(trainArrival: Double, s: Double, Q: Double, alpha: Double)(t: Double): Double = {
    heaviside(t - trainArrival - s) * alpha * heaviside( -(t - trainArrival - s - Q/alpha))
  }

  private def heaviside(x: Double): Double = {
    0.5 * (math.signum(x) + 1)
  }


  val trainParameterGenerator: TINFParametersGenerator = new TINFParametersGenerator(parameters.parameterDistributions, parameters.alphaModels, parameters.parametersByTrainCategory, parameters.sectors)



  def getTINFEstimate(replicas: Int): Vector[Vector[Double]] = {


    (for (i <- 0 to replicas) yield {
      val TINFEstimate: collection.mutable.ArrayBuffer[Double] = collection.mutable.ArrayBuffer.fill(network.edges_TINF.size * parameters.intervals.size)(0)

      //println(timetable._1.get.timeTable.values.collect{case t: TrainWithExchangeVolumes if t.arr.isDefined => {t} }.size)
      timetable._1.get.timeTable.values.collect {
        case t: TrainWithExchangeVolumes if t.arr.isDefined && t.disembarkingPassengers.isDefined => {

          /*if (t.ID.ID == "4060") {
            println("debug")
          }*/

          //println(t.ID.ID, t.disembarkingPassengers.get)
          val tinfEstimate = unloadingLinkCountEstimate(
            (parameters.intervals :+ (parameters.intervals.last + parameters.deltaT)).toVector,
            trainParameterGenerator.generateParameters(t.disembarkingPassengers.get, t.track, if (t.carriages <= 5) {
              "shortTrain"
            } else {
              "longTrain"
            }, t.arr.get.value.toDouble)
          )
          //println(t.ID.ID, t.disembarkingPassengers.get, tinfEstimate.values.flatten.sum)
          /*if (t.ID.ID == "4060") {
            println("debug")
            println(tinfEstimate)
          }*/

          tinfEstimate.foreach(kv => {
            val idxStart: Int = network.edges_TINF_origins.indexWhere(o => {
              o.contains(kv._1) && o.contains(t.track.toString)
            })
            Vector.range(idxStart, TINFEstimate.size, network.edges_TINF.size).zipWithIndex.foreach(idxes => TINFEstimate(idxes._1) = TINFEstimate(idxes._1) + kv._2(idxes._2))
          })

        }
      }
      TINFEstimate.toVector
    }).toVector

  }

  val timetable: (Option[PublicTransportSchedule], Stop2Vertex) = getPTSchedule(config, None, true)





}
