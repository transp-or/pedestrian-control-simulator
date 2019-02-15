import ExploreComplianceFlowSep.{config, mean, results}
import com.typesafe.config.Config
import hubmodel.parseConfigFile
import myscala.math.stats.stats
import myscala.math.stats.bootstrap.bootstrapMSE
import myscala.math.stats.{computeBoxPlotData, computeQuantile, ComputeStats}
import optimization.bruteforce.FlowVariation
import myscala.output.SeqTuplesExtensions.SeqTuplesWriter


object ExploreSensitivityFlows extends App {

  // ******************************************************************************************
  //                    Read CLI arguments and process parameters file
  // ******************************************************************************************

  // Reads the file passed as argument
  val config: Config = parseConfigFile(args)

  def mean(data: Seq[Double]): Double = {data.sum/data.size}
  def median(data: Seq[Double]): Double = computeQuantile(50.0)(data).value.toDouble
  def quant(data: Seq[Double]): Double = computeQuantile(75.0)(data).value.toDouble
  def variance(data: Seq[Double]): Double = data.stats._3

  val flowRatioSensitivity: FlowVariation = new FlowVariation(1.0/1.0, config,1.0,1.0)
  flowRatioSensitivity.runSimulations()

  val results = flowRatioSensitivity.processWrittenResults(variance)

  results.map(r => {
    (r._1, computeBoxPlotData(r._2._1.toVector), bootstrapMSE(r._2._1.toVector, mean))
  }).map(v => (v._1, v._2.mean, v._2.median, v._2.lowerQuartile, v._2.upperQuartile, v._2.lowerWhisker, v._2.upperWhisker , v._2.outliers.size, 0.8*0.2, v._3.parameter, v._3.MSE)).toVector.sortBy(_._1).writeToCSV(config.getString("output.output_prefix") + "-boxplots-travel-times-variance.csv", rowNames=None, columnNames = Some(Vector("flow", "mean", "median", "lowerquartile", "upperquartile", "lowerwhisker", "upperwhiskier", "outliercount", "boxplotwidth")))



  val resultsSplitOD = flowRatioSensitivity.processWrittenResultsByOD(variance)

  resultsSplitOD.flatMap(r => {
    r._2._1.map(u => (r._1, u._1, computeBoxPlotData(u._2.toVector), bootstrapMSE(u._2.toVector, mean)))
  }).groupBy(_._2).foreach(g => g._2.map(t => (t._1, t._3.mean, t._3.median,t._3.lowerQuartile, t._3.upperQuartile,t._3.lowerWhisker, t._3.upperWhisker, t._3.outliers.size, 0.8*0.2, t._4.parameter, t._4.MSE)).toVector.sortBy(_._1).writeToCSV(config.getString("output.output_prefix") + "-" + g._1._1 + "-TO-" + g._1._2 +"-boxplots-travel-times-variance.csv", rowNames=None, columnNames = Some(Vector("flow", "mean", "median", "lowerquartile", "upperquartile", "lowerwhisker", "upperwhisker", "outliercount", "boxplotwidth"))))
}