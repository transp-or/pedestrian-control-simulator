package trackingdataanalysis {
import scala.collection.immutable.NumericRange

package object visualization {

  case class PlotOptions(width: Int = 1000,
                         height: Int = 700,
                         border2HorizontalAxis: Int = 60,
                         border2VerticalAxis: Int = 90,
                         xTickInterval: Double = 1.0,
                         yTickInterval: Double = 1.0,
                         xmin: Option[Double] = None,
                         xmax: Option[Double] = None,
                         ymin: Option[Double] = None,
                         ymax: Option[Double] = None,
                         zmin: Option[Double] = None,
                         zmax: Option[Double] = None)


  def computeHistogramData(x: Iterable[Double], binSize: Double, low: Option[Double], high: Option[Double]): Vector[(Int, Double)] = {
    if (x.isEmpty) {
      throw new IllegalArgumentException("Data for histogram is empty !")
    }

    val xmin: Double = if (low.isDefined) low.get else x.min
    val xmax: Double = if (high.isDefined) high.get else x.max

    // process data
    val intervals: NumericRange[Double] = xmin.to(xmax).by(binSize)

    def binningFunc(v: Double): Int = intervals.indexWhere(_ > v) match {
      case a if a >= 0 => a
      case _ => intervals.length
    }

    x.groupBy(binningFunc).map(kv => kv._1 - 1 -> kv._2.size.toDouble / x.size).toVector.sortBy(_._1)
  }

  def computeHistogramDataWithXValues(x: Iterable[Double], binSize: Double, low: Option[Double], high: Option[Double]): Vector[(Double, Double)] = {
    if (x.isEmpty) {
      throw new IllegalArgumentException("Data for histogram is empty !")
    }

    val xmin: Double = if (low.isDefined) low.get else x.min
    val xmax: Double = if (high.isDefined) high.get else x.max

    // process data
    val intervals: NumericRange[Double] = xmin.to(xmax).by(binSize)

    def binningFunc(v: Double): Int = intervals.indexWhere(_ > v)

    x.groupBy(binningFunc).filterNot(_._1 == -1).map(kv => intervals(kv._1) -> kv._2.size.toDouble / x.size).toVector.sortBy(_._1)
  }
}
}
