import trackingdataanalysis.visualization.HeatMap
import visualization.PlotOptions

object ReadExploreParametersResults extends App {

  {
    val fileTT: String = "t-junction-with-gate_exploration-results-travel-time.csv"

    val bufferedSource = io.Source.fromFile("/home/nicholas/PhD/code/hub-simulator/results-to-keep/" + fileTT)
    val resultsRead: Iterable[Array[Double]] = (for (line <- bufferedSource.getLines) yield {
      println(line.split(",").map(_.trim))
      line.split(",").map(_.trim).map(_.toDouble)
    }).toVector
    bufferedSource.close

    new HeatMap(fileTT + "_heatmap-mean-tt.png", resultsRead.map(r => (r(0), r(1), r(3))), "mean travel time", "constant", "linear")
    new HeatMap(fileTT + "_heatmap-variance-tt.png", resultsRead.map(r => (r(0), r(1), r(4))), "var travel time", "constant", "linear")
    new HeatMap(fileTT + "_heatmap-median-tt.png", resultsRead.map(r => (r(0), r(1), r(5))), "median travel time", "constant", "linear")
  }

  {
    val fileDensity: String = "t-junction-with-gate_exploration-results-density.csv"

    val bufferedSource = io.Source.fromFile("/home/nicholas/PhD/code/hub-simulator/results-to-keep/" + fileDensity)
    val resultsRead: Iterable[Array[Double]] = (for (line <- bufferedSource.getLines) yield {
      println(line.split(",").map(_.trim))
      line.split(",").map(_.trim).map(_.toDouble)
    }).toVector
    bufferedSource.close

    new HeatMap(fileDensity + "_heatmap-mean-density.png", resultsRead.map(r => (r(0), r(1), r(3))), "mean density", "constant", "linear")
    new HeatMap(fileDensity + "_heatmap-variance-density.png", resultsRead.map(r => (r(0), r(1), r(4))), "var density", "constant", "linear")
    new HeatMap(fileDensity + "_heatmap-median-density.png", resultsRead.map(r => (r(0), r(1), r(5))), "median density", "constant", "linear")
  }
}