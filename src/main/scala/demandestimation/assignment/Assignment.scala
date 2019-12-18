package demandestimation.assignment

import java.nio.file.{DirectoryStream, Files, Path, Paths}

import com.typesafe.config.Config
import demandestimation.DemandEstimationParameters
import demandestimation.network.NetworkLausanne
import org.apache.commons.math3.distribution.NormalDistribution
import tools.cells.Vertex
import tools.math.integration.simpsonIntegration
import myscala.output.SeqTuplesExtensions.SeqTuplesWriter

import scala.collection.immutable.ListMap
import scala.collection.mutable.ArrayBuffer

import scala.jdk.CollectionConverters._
import breeze.linalg.{DenseMatrix, DenseVector}

class Assignment(parameters: DemandEstimationParameters, network: NetworkLausanne, config: Config) {


  // ------------------- accumulation matrix

  /** computes residence time
    *
    * @param v
    * @param dist_in
    * @param dist_out
    * @param t_start
    * @param t_end
    * @return
    */
  private def residence_time(v: Double, dist_in: Double, dist_out: Double, t_start: Double, t_end: Double): Double = {

    val t_in = dist_in / v
    val t_out = dist_out / v

    if (t_start <= t_in && t_in <= t_end && t_end <= t_out) {
      t_end - t_in
    }
    else if (t_in <= t_start && t_start <= t_out && t_out <= t_end) {
      t_out - t_start
    }
    else if (t_in <= t_start && t_start <= t_end && t_end <= t_out) {
      t_end - t_start
    }
    else if (t_start <= t_in && t_in <= t_out && t_out <= t_end) {
      t_out - t_in
    }
    else {
      0.0
    }
  }

  /** Helper function for computing the residence time
    *
    * @param t
    * @param v
    * @param dist_in
    * @param dist_out
    * @param tint_minus
    * @param tint_plus
    * @return
    */
  def residence_time_aux(v: Double, dist_in: Double, dist_out: Double, tint_minus: Double, tint_plus: Double)(t: Double): Double = {

    val t_start = tint_minus - t
    val t_end = tint_plus - t
    residence_time(v, dist_in, dist_out, t_start, t_end)
  }


  /** Performs the integration of the pedestrian accumulation
    *
    * @param v
    * @param dist_in
    * @param dist_out
    * @param delta_tint
    * @return
    */
  def accumulation_Gaussian(dist_in: Double, dist_out: Double, delta_tint: Double)(v: Double): Double = {

    val gener_tint_minus: Double = delta_tint * parameters.deltaT
    val gener_tint_plus: Double = (delta_tint + 1) * parameters.deltaT

    parameters.walkingSpeedPDF(v) * simpsonIntegration( residence_time_aux(v, dist_in, dist_out, gener_tint_minus, gener_tint_plus), 0, parameters.deltaT,100)
  }

  /** Helper
    for the accumulation_Gaussian method
    *
    * @param d_in
    * @param d_out
    * @param h_minus_t
    * @return
    */
  def accumulation_aux_Gaussian(d_in: Double, d_out: Double, h_minus_t: Double): Double = {
    (1.0 / (parameters.deltaT * parameters.deltaT)) * simpsonIntegration(accumulation_Gaussian(d_in, d_out, h_minus_t), 0, parameters.speedUpperBound, 100)
  }



  /**   Creates the accumulation assignment matrix
    *
    * @return
    */
  def compute_assg_mat_accumulation: Vector[(Int, Int, Double)] = {

    val R: ArrayBuffer[ArrayBuffer[Double]] = ArrayBuffer.fill(network.areas.size * parameters.max_TT_int, network.routes.size)(0)
    //val S: ArrayBuffer[ArrayBuffer[Double]] = ArrayBuffer.fill(network.areas.size * parameters.intervals.size, network.routes.size * parameters.intervals.size)(0)


  // For each route, we know its distances to the PI, we compute the corresponding matrix entry

    //println(network.dist_routes_areas)

    network.dist_routes_areas.foreach(routeAreaDistance => {
      Vector.range(0, parameters.max_TT_int)
        .foreach(h_minus_t => {
          val tmp = accumulation_aux_Gaussian(routeAreaDistance._2._1, routeAreaDistance._2._2, h_minus_t)
          //println(tmp)
          R(h_minus_t * network.areas.size + routeAreaDistance._1._2)(network.routesIndices(routeAreaDistance._1._1))  = tmp
        })
    })

    (for {
      a <- network.areas.keys
      t <- parameters.intervals.indices
      r <- network.routesIndices
      h <- t until math.min(t + parameters.max_TT_int, parameters.intervals.size)
    } yield {
      (h * network.areas.size + a, t*network.routes.size + r._2, R((h-t)*network.areas.size + a)(r._2))
    }).toVector

    }




   // ---------------------- assignment matrix

  /** This is a simple auxiliary function that will be integrated when computing each element of the assignment matrix.
    * Returns CDF(dist/(h- - t)) - CDF(dist/(h+ - t))
    *
    * @param CDF1
    * @param CDF2
    * @param a
    * @param b
    * @return
    */
  def Gaussian_aux(d_e_r: Double, h_minus: Double, h_plus: Double)(d: Double): Double = {

    val tmp = parameters.walkingSpeedCDF(d_e_r / (h_minus - d)) - parameters.walkingSpeedCDF(d_e_r / (h_plus - d))
    if (tmp < 0.0) {
      tmp + 1.0
    }
    else {
      tmp
    }
  }

  /** Computes an element of the flow assignment matrix, when the speed distribution is normal
    *
    * @param h
    * @param t
    * @param d_e_r
    * @return
    */
  def compute_a_gaussian(h: Double, t: Double, d_e_r: Double):Double = {

  if (h < t){ return 0} // We cannot arrive before leaving


  if ( t - h < 0.00000001 && d_e_r == 0){ return 1 }

  if (d_e_r != -1) {
    //We    do not compute     if the edge is not on the route


    val h_minus = h * parameters.deltaT
    val h_plus = (h + 1) * parameters.deltaT
    val t_minus = t * parameters.deltaT
    val t_plus = (t + 1) * parameters.deltaT


    (1.0 / parameters.deltaT) * simpsonIntegration(Gaussian_aux(d_e_r, h_minus, h_plus), t_minus, t_plus, 100)


  }
  else {
    0
  }

    }

  /** computes the full link assignment matrix (B in paper), for a given network
    * and a given speed distribution (Weidmann, 1993) passed in estim_param
    *
    * @return
    */
  def compute_assg_mat: Vector[(Int, Int, Double)] = {

    //val D: ArrayBuffer[ArrayBuffer[Double]] = ArrayBuffer.fill(network.edgeCollection.size * parameters.max_TT_int, network.routes.size)(0)

    val D: Map[(Int, Int), Double] = network.routes.flatMap(route => {
      //println(route)
      route._2._2.dropRight(1).zip(route._2._2.tail).flatMap(v => {
        //println(v)
        for (h_minus_t <- Vector.range(0, parameters.max_TT_int)) yield {
          //println(network.fullRoutes((route._1._1, v._1)))
          ((h_minus_t * network.edgeCollection.size + network.edgeIndices(v)), (network.routesIndices(route._1))) ->  compute_a_gaussian(h_minus_t, 0, network.fullRoutes((route._1._1, v._1))._1)
        }
      })
    })

    (for {
      r <- network.routesIndices
      t <- parameters.intervals.indices
      e <- network.edgeIndices
      h <- Vector.range(t, math.min(t + parameters.max_TT_int, parameters.intervals.size))
      if D.contains(((h-t)*network.edgeCollection.size + e._2, r._2))
    } yield {
      (h * network.edgeCollection.size + e._2, t*network.routes.size + r._2, D((h-t)*network.edgeCollection.size + e._2, r._2))
    }).toVector

   }


  /**
    * From the full flow assigment matrix, we compute the
    * reduced flow assigment matrix based on the existence of data.
    *
    * @param linkFlowAss
    * @param network
    * @param estim_param
    * @param sensor_edge_dict
    * @return
    */
  def build_flow_assg_mat(linkFlowAss: Map[(Int, Int), Double], sensors: Vector[(Vertex, Vertex)]): Vector[(Int, Int, Double)] = {

    (for {
      t <- parameters.intervals.indices
      sensor <- sensors.indices
    } yield {
      val secondIdx = linkFlowAss.filter(kv => kv._1._1 == t * network.edgeCollection.size + network.edgeIndices(sensors(sensor))).iterator.map(_._1._2)
      secondIdx.map(idx => (t * sensors.size + sensor, idx, linkFlowAss(t * network.edgeCollection.size + network.edgeIndices(sensors(sensor)), idx)))
    }).flatten.toVector
    }


  /** From the full flow assigment matrix, we compute the
    * reduced subroute flow assigment matrix based on the existence of data.
    *
    * @param linkFlowAss
    * @param network
    * @param estim_param
    * @param sensor_routes_dict
    * @return
    */
  def build_ODflow_assg_mat(linkFlowAss: Map[(Int, Int), Double], sensor_routes: ListMap[(Vertex, Vertex), (Double, List[Vertex])], sensors: Vector[(Vertex, Vertex)]): Vector[(Int, Int, Double)]  = {


  for {
    s_r <- sensor_routes.keys
    dep_tint <- parameters.intervals.indices
    arr_tint <- parameters.intervals.indices
  }
    yield {
      val route_idx: Int = network.routesIndices(s_r)
      val sensor_edge_idx: Int = network.edgeIndices((network.routes(s_r)._2.head, network.routes(s_r)._2.tail.head))
      (arr_tint * sensor_routes.size + sensor_edge_idx, dep_tint * network.routes.size + route_idx,  linkFlowAss(arr_tint * network.edgeCollection.size + sensor_edge_idx, dep_tint * network.routes.size + route_idx))

    }

    }.toVector

  def getFilesInDirectory(path: String, extension: String): Vector[Path] = {
    val multipleDemandStream: DirectoryStream[Path] = Files.newDirectoryStream(Paths.get(path), extension)
    val files: Vector[Path] = multipleDemandStream.asScala.toVector
    multipleDemandStream.close()
    files
  }




  val tol: Double = math.pow(10,-7)

  val outputDir: String = {if (config.getString("demandestimation.assignment_dir").last == '/') {
    config.getString("demandestimation.assignment_dir")
  } else {
    config.getString("demandestimation.assignment_dir") + '/'
  } } + "precomputed/"

  println("Precomputed dir is: " + outputDir)

  if (!Files.exists(Paths.get(outputDir))) {
    println("Creating output directory for demand estimation assignment matrices.")
    Files.createDirectory(Paths.get(outputDir))
  }

  /*val (assignmentMatrix: DenseMatrix[Double], assignmentMatrixAccumulation: DenseMatrix[Double]) = {

    if (config.getBoolean("demandestimation.compute_assignment") || getFilesInDirectory(outputDir, ".csv").isEmpty) {

      println("Computing assignment matrices... ")

      val assignmentMatrixAccumulation: DenseMatrix[Double] = DenseMatrix.zeros[Double](network.areas.size * parameters.intervals.size, network.routes.size * parameters.intervals.size)

      this.compute_assg_mat_accumulation.filter(v => math.abs(v._3) > tol).foreach(v => assignmentMatrixAccumulation.update(v._1, v._2, v._3))

      breeze.linalg.csvwrite(new java.io.File(outputDir + "assignment_accumulation_matrix.csv"), assignmentMatrixAccumulation)

      val assignmentMatrix: DenseMatrix[Double] = DenseMatrix.zeros[Double](network.edgeCollection.size * parameters.intervals.size, network.routes.size * parameters.intervals.size)

      this.compute_assg_mat.filter(v => math.abs(v._3) > tol).foreach(v => assignmentMatrix.update(v._1, v._2, v._3))

      breeze.linalg.csvwrite(new java.io.File(outputDir + "assignment_matrix.csv"), assignmentMatrix)

      (assignmentMatrix, assignmentMatrixAccumulation)

    } else {
      println("Reading assignment matrices... ")

      val assignmentMatrixAccumulation: DenseMatrix[Double] = breeze.linalg.csvread(new java.io.File(outputDir + "assignment_accumulation_matrix.csv"))

      val assignmentMatrix: DenseMatrix[Double] = breeze.linalg.csvread(new java.io.File(outputDir + "assignment_matrix.csv"))

      (assignmentMatrix, assignmentMatrixAccumulation)
    }
  }*/

}
