import java.util.concurrent.ThreadLocalRandom
import org.apache.commons.math3.distribution.NormalDistribution
import hubmodel.demand.readStationPTLayout

package object demandestimation {

  class DemandEstimationParameters {


    val startT: Int = 27000
    val endT: Int = 28740
    val deltaT: Int = 60
    val numberIntervals: Int = (endT - startT)/deltaT
    val intervalsInt: Seq[Int] = (startT until endT by deltaT)
    val intervals: Vector[Double] = intervalsInt.map(_.toDouble).toVector




    val walkingSpeedMean: Double = 1.34
    val walkingSpeepVariance: Double = 0.34

    val walkingSpeedPDF: Double => Double = x => new NormalDistribution(walkingSpeedMean, walkingSpeepVariance).density(x)
    val walkingSpeedCDF: Double => Double = x => new NormalDistribution(walkingSpeedMean, walkingSpeepVariance).cumulativeProbability(x)


    // upperbound for the integration over the walking speed distribution
    val speedUpperBound: Double = 10.0

    val max_TT_int = 10 // must be >= 10

    val beta_p2np = 0.9135
    val beta_np2p = 0.95

    val MCIterations: Int = 1

    // files
    val path_ASE_LS_file: String = "lausanne-current/demandestimation/input_data_rand/ASE/2013_01_22_LS.csv"
    val path_ASE_add_file: String = "lausanne-current/demandestimation/input_data_rand/ASE/2013_01_22_add.csv"


    val (parameterDistributions, parametersByTrainCategory, sectors, alphaModels): (Map[String, (Double, Double)], Map[String, Map[Int, Vector[String]]], Map[Int, Vector[String]], Map[String, Map[String, Double]]) =
      readStationPTLayout("lausanne-current/lausanne_station.json")

    println(parameterDistributions)
    println(parametersByTrainCategory)
    println(sectors)
    println(alphaModels)
  }

}
