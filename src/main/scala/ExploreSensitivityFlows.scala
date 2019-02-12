import ExploreComplianceFlowSep.{config, mean, results}
import com.typesafe.config.Config
import hubmodel.parseConfigFile
import myscala.math.stats.bootstrap.bootstrapMSE
import myscala.math.stats.{computeBoxPlotData, computeQuantile}
import optimization.bruteforce.FlowSensitivity
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

  val flowRatioSensitivity: FlowSensitivity = new FlowSensitivity(config)
  flowRatioSensitivity.varyOpposingFlows(1.0/5.0, 9.0)

  val results = flowRatioSensitivity.processWrittenResults(median)

  results.map(r => {
    (r._1._1, computeBoxPlotData(r._2._1.toVector), bootstrapMSE(r._2._1.toVector, mean))
  }).map(v => (v._1, v._2.median, v._2.lowerQuartile, v._2.upperQuartile, v._2.lowerWhisker, v._2.upperWhisker , v._2.outliers.size, 0.8*0.2, v._2.mean, v._3.MSE)).toVector.sortBy(_._1).writeToCSV(config.getString("output.output_prefix") + "-boxplots-travel-times-median.csv", rowNames=None, columnNames = Some(Vector("flow", "median", "lowerquartile", "upperquartile", "lowerwhisker", "upperwhiskier", "outliercount", "boxplotwidth")))



  val resultsSplitOD = flowRatioSensitivity.processWrittenResultsByOD(median)

  resultsSplitOD.flatMap(r => {
    r._2._1.map(u => (r._1, u._1, computeBoxPlotData(u._2.toVector)))
  }).groupBy(_._2).foreach(g => g._2.map(t => (t._1, t._3.mean, t._3.median,t._3.lowerQuartile, t._3.upperQuartile,t._3.lowerWhisker, t._3.upperWhisker, t._3.outliers.size, 0.8*0.2)).toVector.sortBy(_._1).writeToCSV(config.getString("output.output_prefix") + "-" + g._1._1 + "-TO-" + g._1._2 +"-boxplots-travel-times-median.csv", rowNames=None, columnNames = Some(Vector("flow", "mean", "median", "lowerquartile", "upperquartile", "lowerwhisker", "upperwhisker", "outliercount", "boxplotwidth"))))


}