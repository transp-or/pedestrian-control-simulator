package hubmodel.control

/**
  * Parent of all control devices components and control devices themselves. This enforces the definition of a
  * deepCopy method which is required when running simulations in parallel or when doing optimization. Since a new
  * simulation must be created from the previous one, this method makes this job easier.
  */
trait ControlDeviceComponent {

  /**
    * Creates nes instances of the control device component.
    *
    * @return deep copy of the current component
    */
  def deepCopy: ControlDeviceComponent

}
