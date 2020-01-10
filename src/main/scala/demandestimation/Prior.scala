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

  val tinf: DenseVector[Double] = DenseVector[Double](TINFEstimates.head.toArray)


  val routeChoice: RouteChoice = new RouteChoice(parameters, network)
  val dest_flows = routeChoice.generate_dest_flows(data, new TINF(parameters, config, network).timetable._1.get.timeTable.values.toVector)

  val (m_sales: Vector[(Int, Int, Double)], v_sales: Vector[(Int, Double)]) = routeChoice.sales_data_GLS(dest_flows)

  val (m_depflow: Vector[(Int, Int, Double)], v_depflow: Vector[(Int, Double)]) = routeChoice.platform_centroid_dep_flow(dest_flows)

  val m_rsplits: Vector[(Int, Int, Double)] = routeChoice.route_split_GLS( dest_flows)

  val m_agg_rsplits: Vector[(Int, Int, Double)] = routeChoice.agg_cum_route_split_GLS(dest_flows)


  val salesDataRows: Int = 2 * network.Centroids_Shop.size
  val depFlowRows: Int = network.centroid_platform_dict.size
  val rSplitsRows: Int = network.routes.size * parameters.intervals.size
  val rAggSplitsRows: Int = network.centroids.size

  val M: DenseMatrix[Double] = DenseMatrix.zeros[Double](salesDataRows + depFlowRows + rSplitsRows, network.routes.size * parameters.intervals.size)
  val MAgg: DenseMatrix[Double] = DenseMatrix.zeros[Double](salesDataRows + depFlowRows + rAggSplitsRows, network.routes.size * parameters.intervals.size)

  val v: DenseVector[Double] = DenseVector.zeros[Double](salesDataRows + depFlowRows + rSplitsRows)
  val vAgg: DenseVector[Double] = DenseVector.zeros[Double](salesDataRows + depFlowRows + rAggSplitsRows)

  m_sales.foreach(v => {
    M(v._1, v._2) = v._3
    MAgg(v._1, v._2) = v._3
  })

  m_depflow.foreach(v => {
    M(salesDataRows + v._1, v._2) = v._3
    MAgg(salesDataRows + v._1, v._2) = v._3
  })

  m_rsplits.foreach(v => {
    M(salesDataRows + depFlowRows + v._1, v._2) = v._3
  })

  m_agg_rsplits.foreach(v => {
    MAgg(salesDataRows +  depFlowRows + v._1, v._2) = v._3
  })

  v_sales.foreach(d => {
    v(d._1) = d._2
    vAgg(d._1) = d._2
  })

  v_depflow.foreach(d => {
    v(salesDataRows +  d._1) = d._2
    vAgg(salesDataRows +  d._1) = d._2
  })

}
