package demandestimation

import java.util.concurrent.ThreadLocalRandom

import hubmodel.demand.TrainArrival

case class TrainParameters(trainArrival: Double, deadTime: Double, alpha: Double, Q: Double, ratios: Iterable[(String, Double)])

class TINFParametersGenerator(TINFParamDist: Map[String, (Double, Double)], alphaModels: Map[String, Map[String, Double]], parametersByTrainCategory: Map[String, Map[Int, Vector[String]]], sectors: Map[Int, Vector[String]]) {

  /** Takes the data from distribution specs from normalFits and returns one alpha.
    * Samples from the zero-mean distribution.
    *
    * @param self
    * @param Q
    * @param platform
    * @return
    */
  def sample_cap_flow_rate(Q: Double, platform: Int): Double = {

    if (platform == 70) {
       TINFParamDist("alpha70")._1
    }
    else if (platform == 1) {
       TINFParamDist("alpha1")._1
    }
    else if (platform == 9) {
       TINFParamDist("alpha9")._1
    }
    else if (platform == 7 || platform == 8) {

      if (Q > alphaModels("red")("Qc")) {
         alphaModels("red")("a") * alphaModels("red")("Qc") + alphaModels("red")("b") + TINFParamDist("alphaFit")._2 //* ThreadLocalRandom.current.nextGaussian()
      }
      else if (0 < Q && Q < alphaModels("red")("Qc")) {
         alphaModels("red")("a") * Q + alphaModels("red")("b") + TINFParamDist("alphaFit")._2 //* ThreadLocalRandom.current.nextGaussian()
      }
      else {throw new IllegalAccessError("Case should not happend ! ")}
    }
    else {

      if (Q > alphaModels("full")("Qc")) {
         alphaModels("full")("a") * alphaModels("full")("Qc") + alphaModels("full")("b") + TINFParamDist("alphaFit")._2 //* ThreadLocalRandom.current.nextGaussian()
      }
      else if (0 < Q && Q < alphaModels("full")("Qc")) {
         alphaModels("full")("a") * Q + alphaModels("full")("b") + TINFParamDist("alphaFit")._2 //* ThreadLocalRandom.current.nextGaussian()
      }
      else {
        throw new IllegalAccessError("Case should not happend ! ")
      }
    }
  }

  /** Normalizes the distribution between the access ramps.
    * The sum must be one to not "loose" any pedestrians.
    *
    * @param self
    * @param normalSpecs
    * @param sectorNames
    * @return
    */
  def sample_flow_distribution(normalSpecs: Map[String, (Double , Double)]): Iterable[(String, Double)] = {

    //val rawRatios: Iterable[(String, Double)] = normalSpecs.map(norm => (norm._1, math.max(0.0,norm._2._1 + norm._2._2 * ThreadLocalRandom.current.nextGaussian() )))
    val rawRatios: Iterable[(String, Double)] = normalSpecs.map(norm => (norm._1, math.max(0.0,norm._2._1 + norm._2._2 * ThreadLocalRandom.current.nextDouble(1,1.0000001) )))

    rawRatios.iterator.map(r => (r._1, r._2 / rawRatios.map(_._2).sum)).toVector
    }


  /** Returns a scalar from the normal distribution with parameters specified in normalFits DataFrame
    *
    * @return
    */
  def sample_dead_time(): Double = {

  var dead_time: Double = -1.0
  while (dead_time < 0) {
    dead_time = TINFParamDist("s")._2 /** ThreadLocalRandom.current.nextGaussian()*/ + TINFParamDist("s")._1
  }
  dead_time
    }


  def generateParameters(Q: Double, track: Int, trainType: String, trainArrivalTime: Double): TrainParameters = {
    TrainParameters(trainArrivalTime, sample_dead_time(), sample_cap_flow_rate(Q, track), Q, sample_flow_distribution(parametersByTrainCategory(trainType)(track).zip(sectors(track)).map(k => k._2 -> TINFParamDist(k._1)).toMap))
  }

}
