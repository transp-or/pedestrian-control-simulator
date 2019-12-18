package tools.math

package object integration {

  type Method = (Double=>BigDecimal, Double, Double) => BigDecimal

  def simpson(f:Double=>BigDecimal, a:Double, b:Double): BigDecimal = {(f(a)+(4)*f((a+b)/2)+f(b))/6 }

  def integrate(f:Double=>BigDecimal, a:Double, b:Double, steps:Double, m:Method): Double = {
    val delta: BigDecimal= BigDecimal((b-a)/steps)
    (delta*(BigDecimal(a) until BigDecimal(b) by delta).foldLeft(BigDecimal(0.0))((s,x) => s+m(f, x.toDouble, x.toDouble+delta.toDouble))).toDouble
  }

  def simpsonIntegration(f:Double => Double, a: Double, b: Double, steps: Int = 1000): Double = {
    val fBG: Double => BigDecimal = x => BigDecimal(f(x))
    integrate(fBG, a, b, steps, simpson)
  }

}
