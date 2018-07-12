package trackingdataanalysis.pedtrack.visiosafe

import breeze.linalg.{DenseMatrix, DenseVector}
import breeze.stats.regression.{LeastSquaresRegressionResult, leastSquares}

class CalibratePIController {

  def estimateOutflowFraction(outflow: DenseVector[Double], totalFlow: DenseMatrix[Double]): Double = {
    val result: LeastSquaresRegressionResult = leastSquares(totalFlow, outflow)
    //assert(result.coefficients.activeSize == 1.0)
    result.coefficients(0)
  }

  def computeControllerParams(inflow: DenseVector[Double], outflow: DenseVector[Double], density: DenseVector[Double], totalFlow: DenseVector[Double]): Unit = {
    println(inflow.size, outflow.size, density.size, totalFlow.size)
    val targetDensity: Double = 1.2
    val targetInflow: Double = 45.0
    val outflowFraction: Double = estimateOutflowFraction(outflow, totalFlow.asDenseMatrix.t)

    val density_kp = density(1 until density.size) - targetDensity
    val density_k = density(0 until density.size-1) - targetDensity
    val controlledInflow_k = inflow(0 until inflow.size-1) - targetInflow


    val result: LeastSquaresRegressionResult = leastSquares(DenseMatrix(density_k, controlledInflow_k).t, density_kp)
    val mu: Double = result.coefficients(0)
    val ksi:Double = result.coefficients(1)
    println("mu=" + mu + ", ksi=" + ksi)
    println("Kp=" + mu/ksi + ", Ki=" + (1.0-mu)/ksi)
  }

}
