package tools.math

import scala.annotation.tailrec

package object integration {

  type Method = (Double=>BigDecimal, Double, Double) => BigDecimal

  def simpson(f:Double=>BigDecimal, a:Double, b:Double): BigDecimal = {(f(a)+4.0*f((a+b)/2.0)+f(b))/6.0 }

  def integrate(f:Double=>BigDecimal, a:Double, b:Double, steps:Double, m:Method): Double = {
    val delta: BigDecimal= BigDecimal((b-a)/steps)
    (delta*(BigDecimal(a) until BigDecimal(b) by delta).foldLeft(BigDecimal(0.0))((s,x) => s+m(f, x.toDouble, x.toDouble+delta.toDouble))).toDouble
  }

  def simpsonIntegration(f:Double => Double, a: Double, b: Double, steps: Int = 1000): Double = {
    val fBG: Double => BigDecimal = x => BigDecimal(f(x))
    integrate(fBG, a, b, steps, simpson)
  }


  def rectangleIntegration(f: Vector[(Double, Double)], a: Double, b: Double): Double = {

    ((f.head._1 - a) + 0.5*(f.tail.head._1 - f.head._1) * f.head._2 +
    (b - f.last._1) + 0.5*(f.last._1 - f.dropRight(1).last._1) * f.last._2 +
    f.sliding(3).foldLeft(BigDecimal(0.0))((s,x) => s + (x(1)._2 * 0.5*(x(2)._1 - x(0)._1)))).toDouble

  }

}
