package demandestimation

import breeze.linalg.{DenseMatrix, DenseVector}
import com.typesafe.config.Config
import demandestimation.assignment.Assignment
import demandestimation.network.NetworkLausanne
import hubmodel.parseConfigFile
import myscala.math.stats.ComputeStats
import org.jgrapht.alg.interfaces.MinimumVertexCoverAlgorithm.VertexCover


object Debug extends App {

  // parses the config file and checks if the output dir exists.
  val config: Config = parseConfigFile(args)

  val network = new NetworkLausanne

  val parameters = new DemandEstimationParameters

  //println(network.routes((network.positions("NW"), network.positions("SW"))))

  //println(network.areas)

  //println(network.dist_routes_areas((network.positions("NW"), network.positions("SW")), 0))

  val assignment = new Assignment(parameters, network, config)


  //println(assignment.accumulation_Gaussian(0, 1, 1)(1.0)) // ok

  //println(assignment.accumulation_aux_Gaussian(4.2206555, 7.1206555, 0)) // ok

  //val res1 = assignment.compute_assg_mat_accumulation

  //println(res1.filter(_._3 > 0.0))

  //val res2 = assignment.compute_assg_mat

  //println(res2.filter(_._3 > 0.0))


  val tinf = new TINF(parameters, config, network)

  //println(tinf.unloadingLinkCountEstimate(parameters.intervals.toVector, TrainParameters(27960,1.0,5,200,Vector(("A", 0.25) ,("B", 0.25), ("C", 0.5)))).mkString("\n")) // ok

  //println(tinf.PWL(0,1.5,100,4)(26)) // ok

  //val tinfData = tinf.getTINFEstimate(1)

  //println(tinfData.head.mkString("\n")) // ok

  //println(tinfData.transpose.map(d => d.statistics).map(stats => stats.mean).sum) // ok

  val data = new MeasurementData(parameters, network)

  //println(data.ASEData) // ok

  //println(data.f_hat) // ok



  val prior = new Prior(parameters, network, config, data)

  //print(prior.dest_flows) // ok
  //println(prior.m_sales) // ok
  //println(prior.v_sales) // ok

  //println(prior.m_depflow) // ok

  //println(prior.v_depflow) // ok

  // println(prior.m_rsplits.filter(_._1 < 5 )) // ok

  // println(prior.m_agg_rsplits)

  // collecting components

  // TINF
  val tinfData = prior.tinf
  val tinfAssigMat = assignment.build_flow_assg_mat(network.edges_TINF)

  // data
  val mData = data.f_hat
  val mDataAssigMat = assignment.build_flow_assg_mat(network.edges_ASE)

  // historical data
  val hAggData = prior.vAgg
  val hAggAssigMat = prior.MAgg

  val mat = DenseMatrix.vertcat(tinfAssigMat,DenseMatrix.vertcat(mDataAssigMat, hAggAssigMat))
  val vec = DenseVector.vertcat(tinfData,DenseVector.vertcat(mData, hAggData))

  val opt = new breeze.optimize.linear.NNLS()
  val res = opt.minimizeAndReturnState(mat, vec)
  println(res.converged)
  println(res.x)
  breeze.linalg.csvwrite(new java.io.File("result.csv"), DenseMatrix(res.x))


}
