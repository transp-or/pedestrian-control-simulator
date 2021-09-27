package hubmodel.control

import tools.Time

/**
  * Parent of all control devices components and control devices themselves. This enforces the definition of a
  * deepCopy method which is required when running simulations in parallel or when doing optimization. Since a new
  * simulation must be created from the previous one, this method makes this job easier as this enforces the
  * creation of the deep copy method.
  */
trait ControlDeviceComponent {

  /**
    * Creates nes instances of the control device component.
    *
    * @return deep copy of the current component
    */
  def deepCopy: ControlDeviceComponent
}

abstract class MeasurementError
case class FlowLineRandomMeasurementError(varianceInterger: Int) extends MeasurementError {
  require(varianceInterger > 0 && varianceInterger % 2 == 1)

}
case class DensityRandomMeasurementError(varianceFraction: Double) extends MeasurementError {
  require(varianceFraction > 0.0 && varianceFraction <= 1.0)

}

abstract class MeasurementDevice(val measurementError: Option[MeasurementError]) extends ControlDeviceComponent

/** Parent to all control policies. This is used when the policy is fixed from outside the device itself. The main
  * usage of this class is for the optimization procedure when the control policy is computed by the optimization
  * framework.
  *
  * @param start start of the control policy
  */
abstract class ControlDevicePolicy(val start: Time, val name: String) {

  /** human readable name of the policy.
    *
    * @return
    */
  def nameToString: String

  /** Decision variable which is optimized
    *
    */
  val decisionVariable: Double
}

/** Ordering by start time of the [[ControlDevicePolicy]] classes.
  *
  */
object ControlDevicePolicy {

  // Note that because `Ordering[A]` is not contravariant, the declaration
  // must be type-parametrized in the event that you want the implicit
  // ordering to apply to subclasses of `ControlDevicePolicy`.
  /*implicit def orderingByStart[A <: ControlDevicePolicy]: Ordering[A] =
    Ordering.by(e => e.start* -1.0)*/

  implicit def orderingByStartAndName[A <: ControlDevicePolicy]: Ordering[A] =
    Ordering.by(e => (e.name, e.start))
}

class ControlDeviceData(val name: String)
