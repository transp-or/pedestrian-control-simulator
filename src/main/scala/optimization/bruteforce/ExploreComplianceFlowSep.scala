package optimization.bruteforce

import com.typesafe.config.Config
import hubmodel.parseConfigFile
import myscala.math.stats.bootstrap.bootstrapMSE
import myscala.math.stats.{computeBoxPlotData, computeQuantile}
import myscala.output.SeqTuplesExtensions.SeqTuplesWriter
import optimization.bruteforce.explorations.ComplianceVariation

object ExploreComplianceFlowSep extends App {

  // ******************************************************************************************
  //                    Read CLI arguments and process parameters file
  // ******************************************************************************************

  // Reads the file passed as argument
  val config: Config = parseConfigFile(args)

  val complianceIntervals = 0.1

  val complianceAnalysis: ComplianceVariation = new ComplianceVariation(complianceIntervals, config, 0.5)

  complianceAnalysis.runSimulations()

  def mean(data: Seq[Double]): Double = {
    data.sum / data.size
  }

  def median(data: Seq[Double]): Double = computeQuantile(50.0)(data).value.toDouble

  def quant(data: Seq[Double]): Double = computeQuantile(90.0)(data).value.toDouble


  val results = complianceAnalysis.processWrittenResults(quant)


  results.map(r => {
    /*
        // creates histograms
        new Histogram(config.getString("output.output_prefix") + "tt-histogram_" + r._1 + ".png",
          r._2._1,
          0.5,
          "TT [pax/m^2]",
          "Histogram of TT for compliance: " + r._1,
          opts = PlotOptions(xmax = Some(50), xmin = Some(15), ymax = Some(0.05)))
    */
    // creates boxplot data
    (r._1, computeBoxPlotData(r._2._1.toVector), bootstrapMSE(r._2._1.toVector, mean))
  }).map(v => (v._1, v._2.median, v._2.lowerQuartile, v._2.upperQuartile, v._2.lowerWhisker, v._2.upperWhisker, v._2.outliers.size, 0.8 * complianceIntervals, v._2.mean, v._3.MSE)).toVector.sortBy(_._1).writeToCSV(config.getString("output.output_prefix") + "-boxplots-travel-times-90quantile.csv", rowNames = None, columnNames = Some(Vector("compliance", "median", "lowerquartile", "upperquartile", "lowerwhisker", "upperwhiskier", "outliercount", "boxplotwidth")))


  val resultsByOD = complianceAnalysis.processWrittenResultsByOD(quant)

  resultsByOD.flatMap(r => {
    r._2._1.map(u => (r._1, u._1, computeBoxPlotData(u._2.toVector)))
  }).groupBy(_._2).foreach(g => g._2.map(t => (t._1, t._3.mean, t._3.median, t._3.lowerQuartile, t._3.upperQuartile, t._3.lowerWhisker, t._3.upperWhisker, t._3.outliers.size, 0.8 * complianceIntervals)).toVector.sortBy(_._1).writeToCSV(config.getString("output.output_prefix") + "-" + g._1._1 + "-TO-" + g._1._2 + "-boxplots-travel-times-90quantile.csv", rowNames = None, columnNames = Some(Vector("compliance", "mean", "median", "lowerquartile", "upperquartile", "lowerwhisker", "upperwhisker", "outliercount", "boxplotwidth"))))

}
