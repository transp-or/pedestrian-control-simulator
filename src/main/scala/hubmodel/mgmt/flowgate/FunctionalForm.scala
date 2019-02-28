package hubmodel.mgmt.flowgate

abstract class Measurement
case class Density(d: Double) extends Measurement
case class BidirectionalFlow(f1: Double, f2: Double) extends Measurement

abstract class Output
case class Flow(f: Double) extends Output
case class SeparatorPositionFraction(r: Double) extends Output {
  if (r < 0.0 || r > 1.0) {throw new IllegalArgumentException("Position of flow separator cannot be outside of [0,1] ! r=" + this.r)}
}

abstract class FunctionalForm[T <: Measurement, U <: Output](val functionalForm: T => U)

case class FunctionalFormDensity(f: Density => Flow) extends FunctionalForm[Density, Flow](f)

