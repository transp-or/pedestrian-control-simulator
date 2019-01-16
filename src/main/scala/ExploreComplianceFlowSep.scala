import RunSimulation.args
import com.typesafe.config.{Config, ConfigFactory}
import hubmodel.parseConfigFile
import optimization.bruteforce.ComplianceVariation
import myscala.math.stats.ComputeStats
import myscala.math.stats.computeQuantiles
import myscala.output.SeqTuplesExtensions.SeqTuplesWriter
import trackingdataanalysis.visualization.{Histogram, PlotOptions}

object ExploreComplianceFlowSep extends App {

  // ******************************************************************************************
  //                    Read CLI arguments and process parameters file
  // ******************************************************************************************

  // Reads the file passed as argument
  val config: Config = parseConfigFile(args)

  val complianceIntervals = 0.025

  val complianceAnalysis: ComplianceVariation = new ComplianceVariation(complianceIntervals, config, 0.5)

  complianceAnalysis.runSimulations()

  val results = complianceAnalysis.processWrittenResults

  results.map(r => {
    new Histogram(config.getString("output.output_prefix") + "tt-histogram_" + r._1 + ".png",
      r._2._2,
      0.5,
      "TT [pax/m^2]",
      "Histogram of TT for compliance: " + r._1,
      opts = PlotOptions(xmax = Some(50), xmin = Some(15), ymax = Some(0.05)))
    (r._1, computeBoxPlotData(r._2._2.toVector))
  }).map(v => (v._1, v._2._1, v._2._2, v._2._3, v._2._4, v._2._5 , v._2._6.size, 0.8*complianceIntervals)).toVector.writeToCSV(config.getString("output.output_prefix") + "_boxplots_travel_times.csv", rowNames=None, columnNames = Some(Vector("compliance", "median", "lowerquartile", "upperquartile", "lowerwhisker", "upperwhiskier", "outliercount", "boxplotwidth")))

  def computeBoxPlotData(data: Seq[Double]): (Double, Double, Double, Double, Double, Seq[Double]) = {

    val quarters = computeQuantiles(Vector(25.0, 50.0, 75.0))(data)
    val lowerQuartile = quarters.values(0)
    val median = quarters.values(1)
    val upperQuartile = quarters.values(2)

    val lowerWhisker = data.map(_ - (lowerQuartile - (upperQuartile - lowerQuartile) * 1.5)).filter(_ >= 0).min + lowerQuartile - (upperQuartile - lowerQuartile) * 1.5
    val upperWhisker = data.map(_ - (upperQuartile + (upperQuartile - lowerQuartile) * 1.5)).filter(_ <= 0).max + upperQuartile + (upperQuartile - lowerQuartile) * 1.5

    (median, lowerQuartile, upperQuartile, lowerWhisker, upperWhisker, data.filter(v => v < lowerWhisker || v > upperWhisker)
    )
  }

}