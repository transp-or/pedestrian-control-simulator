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

}
