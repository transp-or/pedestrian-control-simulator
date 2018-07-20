import trackingdataanalysis.visualization.HeatMap
import visualization.PlotOptions

object ReadExploreParametersResults extends App {

  val file: String = "t-junction-with-gate_exploration-results"

  val bufferedSource = io.Source.fromFile("/home/nicholas/PhD/code/hub-simulator/results-to-keep/" + file + ".csv")
  val resultsRead: Iterable[Array[Double]] = (for (line <- bufferedSource.getLines) yield {
    println(line.split(",").map(_.trim))
    line.split(",").map(_.trim).map(_.toDouble)
  }).toVector
  bufferedSource.close

  //results.map(r => (r._1, r._2, r._3._1, r._3._2, r._3._3, r._3._4, r._3._5, r._3._6)).toVector.writeToCSV(config.getString("output.output_prefix") + "_exploration-results.csv")
  new HeatMap(file + "_heatmap-mean-tt.png", resultsRead.map(r => (r(0), r(1), r(3))), "mean travel time", "constant", "linear", PlotOptions(zmin=Some(27), zmax=Some(35)))
  new HeatMap(file + "_heatmap-variance-tt.png", resultsRead.map(r => (r(0), r(1), r(4))), "var travel time", "constant", "linear")
  new HeatMap(file + "_heatmap-median-tt.png", resultsRead.map(r => (r(0), r(1), r(5))), "median travel time", "constant", "linear")
}