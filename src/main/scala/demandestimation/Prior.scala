package demandestimation

import breeze.linalg.{DenseMatrix, DenseVector}
import com.typesafe.config.Config
import demandestimation.network.NetworkLausanne
import myscala.math.stats.{ComputeStats, Statistics}

class Prior(parameters: DemandEstimationParameters, network: NetworkLausanne, config: Config, data: MeasurementData) {

  val TINFEstimates: Vector[Vector[Double]] = Vector.range(0, parameters.MCIterations).map(i => new TINF(parameters, config, network).getTINFEstimate(1).head)
  val TINFStats: Vector[Statistics[Double]] = Vector.range(0, TINFEstimates.head.size).map(d => {
    TINFEstimates.map(tinf => tinf(d)).statistics
  })

  val routeChoice: RouteChoice = new RouteChoice(parameters, network)
  val dest_flows = routeChoice.generate_dest_flows(data, new TINF(parameters, config, network).timetable._1.get.timeTable.values.toVector)

  val (m_sales: Vector[(Int, Int, Double)], v_sales: Vector[(Int, Double)]) = routeChoice.sales_data_GLS(dest_flows)
  val (m_depflow: Vector[(Int, Int, Double)], v_depflow: Vector[(Int, Double)]) = routeChoice.platform_centroid_dep_flow(dest_flows)
  val m_rsplits: Vector[(Int, Int, Double)] = routeChoice.route_split_GLS( dest_flows)
  val m_agg_rsplits: Vector[(Int, Int, Double)] = routeChoice.agg_cum_route_split_GLS(dest_flows)

  val MAgg: DenseMatrix[Double] = DenseMatrix.zeros[Double](m_sales.size + m_depflow.size + m_agg_rsplits.size, network.routes.size * parameters.intervals.size)
  val M: DenseMatrix[Double] = DenseMatrix.zeros[Double](m_sales.size + m_depflow.size + m_rsplits.size, network.routes.size * parameters.intervals.size)

  val v: DenseVector[Double] = DenseVector.zeros[Double](v_sales.size + v_depflow.size + m_rsplits.size)
  val vAgg: DenseVector[Double] = DenseVector.zeros[Double](v_sales.size + v_depflow.size + m_agg_rsplits.size)

  m_sales.foreach(v => {
    M(v._1, v._2) = v._3
    MAgg(v._1, v._2) = v._3
  })

  m_depflow.foreach(v => {
    M(m_sales.size + v._1, v._2) = v._3
    MAgg(m_sales.size + v._1, v._2) = v._3
  })

  m_rsplits.foreach(v => {
    M(m_sales.size +  + m_depflow.size + v._1, v._2) = v._3
  })

  m_agg_rsplits.foreach(v => {
    MAgg(m_sales.size +  m_depflow.size + v._1, v._2) = v._3
  })

  v_sales.foreach(d => {
    v(d._1) = d._2
    vAgg(d._1) = d._2
  })

  v_depflow.foreach(d => {
    v(m_sales.size +  d._1) = d._2
    vAgg(m_sales.size +  d._1) = d._2
  })

}
