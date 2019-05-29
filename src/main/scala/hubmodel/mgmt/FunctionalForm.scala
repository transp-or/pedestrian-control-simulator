package hubmodel.mgmt

import hubmodel.tools.exceptions.{IllegalFlowSeparatorPosition, IllegalPhysicalQuantity}

abstract class Measurement

case class Density(d: Double) extends Measurement

case class BidirectionalFlow(f1: Double, f2: Double) extends Measurement {
  override def toString: String = "f1=" + this.f1 + ", f2=" + this.f2
}

abstract class Output

case class Flow(f: Double) extends Output {
  if (f < 0.0) {
    throw new IllegalPhysicalQuantity("Flow cannot be negative ! f=" + this.f)
  }
}

case class SeparatorPositionFraction(r: Double) extends Output {
  if (r < 0.0 || r > 1.0) {
    throw new IllegalFlowSeparatorPosition("Position of flow separator cannot be outside of [0,1] ! r=" + this.r, this.r)
  }
}

abstract class FunctionalForm[T <: Measurement, U <: Output](functionalForm: T => U)

case class FunctionalFormFlowSeparator(f: BidirectionalFlow => SeparatorPositionFraction) extends FunctionalForm(f)

case class FunctionalFormGating(f: Density => Flow) extends FunctionalForm(f)

