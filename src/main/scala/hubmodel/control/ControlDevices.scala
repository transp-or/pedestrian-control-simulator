package hubmodel.control

import hubmodel.control.amw.{FlowLineWithFraction, MovingWalkwayAbstract, MovingWalkwayWithDensityMeasurement}
import hubmodel.control.flowgate.{BinaryGate, FlowGate, FlowGateFunctional}
import hubmodel.control.flowsep.{FlowLine, FlowSeparator, FlowSeparatorParameters}
import hubmodel.ped.PedestrianNOMAD
import hubmodel.supply.graph.{GraphContainer, RouteGraph}
import tools.Time
import tools.cells.DensityMeasuredArea
import tools.exceptions.IllegalSimulationInput

/**
  * Container for all the control devices used in the pedestrian simulator. This makes copying and optimizing the
  * control strategies easier. There is no need to manage individually each control strategy.
  *
  * @param monitoredAreas      locations where KPI ars monitored
  * @param amws                accelerated moving walkways
  * @param flowGates           gates controlling flows
  * @param binaryGates         binary gates (open or closed)
  * @param flowSeparators      dynamic flow separators
  * @param fixedFlowSeparators indicator if the flow separators are fixed
  */
class ControlDevices(val monitoredAreas: Iterable[DensityMeasuredArea], val amws: Iterable[MovingWalkwayAbstract], val amwsMode: (String, String), val flowGates: Iterable[FlowGate], val binaryGates: Iterable[BinaryGate], val flowSeparators: Iterable[FlowSeparator[_, _]], val fixedFlowSeparators: Boolean, val flowLines: Vector[FlowLine], val flowSepParams: Option[Seq[FlowSeparatorParameters[_, _]]] = None) extends ControlDeviceComponent {

  // Incompatible setup: flow gates exist but no areas to measure density exist
  if (flowGates.nonEmpty && monitoredAreas.isEmpty) {
    throw new IllegalSimulationInput("flow gates present but no monitored area")
  }

  // Incompatible setup: binary gates exist but no measurement area exists
  if (binaryGates.nonEmpty && monitoredAreas.isEmpty) {
    throw new IllegalSimulationInput("binary gates present but no monitored area, or vice-versa")
  }

  // Incompatible setup: flow gates are referencing a non-existent monitored area
  if (flowGates.exists(fg => !monitoredAreas.map(_.name).toVector.contains(fg.monitoredArea))) {
    throw new IllegalSimulationInput("flow gate controlled area not found in list of monitored areas")
  }

  /**
    * Copies the container with the control devices.
    *
    * @return deep copy of the current component
    */
  override def deepCopy: ControlDevices = {
    new ControlDevices(monitoredAreas.map(_.deepCopy), amws.map(_.deepCopy), this.amwsMode, flowGates.map(_.deepCopy), binaryGates.map(_.deepCopy), flowSeparators.map(_.deepCopy), fixedFlowSeparators, this.flowLines.map(_.deepCopy))
  }


  /**
    * Copies the container with the control devices and includes the state of the devices
    *
    * @return deep copy of the current component
    */
  def deepCopyWithState(t: => Time, population: Iterable[PedestrianNOMAD]): ControlDevices = {
    new ControlDevices(monitoredAreas.map(_.deepCopy), amws.map(_.deepCopyWithState(population)), this.amwsMode, flowGates.map(_.deepCopyWithState(t, population)), binaryGates.map(_.deepCopy), flowSeparators.map(_.deepCopyWithState), fixedFlowSeparators, this.flowLines.map(_.deepCopy))
  }

  /**
    * Copies the full control devices but changes the functional forms of the flow gates.
    *
    * @param f new functional form to use
    * @tparam T measurement type
    * @tparam U output type
    * @return new set of control devices
    */
  def deepCopyModifyFlowGates[T <: Measurement, U <: Flow](f: FunctionalForm[T, U]): ControlDevices = {
    new ControlDevices(monitoredAreas.map(_.deepCopy), amws.map(_.deepCopy), this.amwsMode, flowGates.map(fg => fg match {
                case fgFunc: FlowGateFunctional[_, _] => {
                  fgFunc.deepCopy(f)
                }
                case fg: FlowGate => {
                  fg.deepCopy
                }
              }), binaryGates.map(_.deepCopy), flowSeparators.map(_.deepCopy), fixedFlowSeparators, this.flowLines.map(_.deepCopy))
  }

  def deepCopyModifyMonitoredAreas(rho: Double): ControlDevices = {
    new ControlDevices(monitoredAreas.map(_.deepCopyChangeTargetDensity(rho)), amws.map(_.deepCopy), this.amwsMode, flowGates.map(_.deepCopy), binaryGates.map(_.deepCopy), flowSeparators.map(_.deepCopy), fixedFlowSeparators, this.flowLines.map(_.deepCopy))
  }

  /**
    * Copies the control devices and changes the flow separators functional form.
    *
    * @param f new functional form to use
    * @tparam T measurement type
    * @tparam U output type
    * @return new set of control devices
    */
  def deepCopyModifyFlowSeparators[T <: Measurement, U <: SeparatorPositionFraction](f: FunctionalForm[T, U]): ControlDevices = {
    new ControlDevices(monitoredAreas.map(_.deepCopy), amws, this.amwsMode, flowGates.map(_.deepCopy), binaryGates.map(_.deepCopy), flowSeparators.map(_.deepCopy(f)), fixedFlowSeparators, this.flowLines.map(_.deepCopy))
  }

  def deepCopyModifyMovingWalkways[T <: Measurement, U <: MovingWalkwaySpeed](P: Double, I: Double, graph: GraphContainer, flowLines: Vector[FlowLine], areas: Vector[DensityMeasuredArea]): ControlDevices = {
    new ControlDevices(areas, amws.collect{case w: MovingWalkwayWithDensityMeasurement[_,_] => w.deepCopyPIGains(graph, flowLines, areas, P, I)}, this.amwsMode, Vector(), Vector(), Vector(), fixedFlowSeparators, flowLines)
  }
}
