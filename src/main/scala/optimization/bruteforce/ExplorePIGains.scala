package optimization.bruteforce

import java.io.File

import com.typesafe.config.Config
import hubmodel.parseConfigFile
import hubmodel.results.readResultsJson
import myscala.math.stats.bootstrap.bootstrapMSE
import myscala.math.stats.{computeBoxPlotData, computeQuantile}
import myscala.math.stats.ComputeStats
import trackingdataanalysis.visualization.HeatMap

object ExplorePIGains extends App {

  // ******************************************************************************************
  //                    Read CLI arguments and process parameters file
  // ******************************************************************************************

  // Reads the file passed as argument
  val config: Config = parseConfigFile(args)

  val analysis: PIGainsExploration = new PIGainsExploration(1.2,1.5,0.2,0.5,0.05, config)

  //analysis.runSimulations()

  val resultFiles = groupResultsFiles

  def groupResultsFiles: Map[(Double, Double, String), Vector[File]] = { // reads the files and process the data
    new File(config.getString("output.dir")).listFiles.filter(_.isFile).toVector.filter(f => analysis.extractFileGroup2Parameters(f).isDefined).map(f => (analysis.extractFileGroup2Parameters(f).get, f)).groupBy(a => a._1).mapValues(_.map(_._2)).to(Map)
  }

  def computeDensityIntegral(individiualDensityData: Map[(String, String), Vector[(tools.Time, Vector[Double])]]): Double = {
    individiualDensityData.toVector.flatMap(a => a._2.map(d => d._1.value.toDouble * {if (d._2.isEmpty){0.0} else {math.max(0.0, computeQuantile(75)(d._2).value - 1.2)}})).sum
  }


  val heatmap = resultFiles.toVector.map(files => {
    val r = readResultsJson(config.getString("output.dir"), files._1._3 + "_params_", Vector()).toVector
    val tt = r.flatMap(_.tt.map(_.tt))
    val data = tt.statistics
    (files._1._1, files._1._2, r.map(d => computeDensityIntegral(d.monitoredAreaIndividualDensity.get)).sum/r.size.toDouble)
    //(files._1._1, files._1._2, computeQuantile(25)(tt).value)
    //(files._1._1, files._1._2, data.mean)
  })

  println(resultFiles)
  println(heatmap.sortBy(_._3))

  new HeatMap(config.getString("output.output_prefix") + "_heatmap-mean-tt.png", heatmap, "mean travel time", "P", "I", "mean travel time")



  /*
    def mean(data: Seq[Double]): Double = {
      data.sum / data.size
    }

    def median(data: Seq[Double]): Double = computeQuantile(50.0)(data).value.toDouble

    def quant(data: Seq[Double]): Double = computeQuantile(90.0)(data).value.toDouble


    val results = analysis.processWrittenResults(quant)


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
  */
}
