package hubmodel.mgmt

import hubmodel.mgmt.flowgate._
import hubmodel.mgmt.flowsep.FlowSeparator
import hubmodel.supply.graph._
import hubmodel.tools.ControlDevicesException
import hubmodel.tools.cells.DensityMeasuredArea


class ControlDevices(val monitoredAreas: Iterable[DensityMeasuredArea],
                     val amws: Iterable[MovingWalkway],
                     val flowGates: Iterable[FlowGate],
                     val binaryGates: Iterable[BinaryGate],
                     val flowSeparators: Iterable[FlowSeparator[_, _]],
                     val fixedFlowSeparators: Boolean) {

  if (flowGates.nonEmpty && monitoredAreas.isEmpty) {
    throw new ControlDevicesException("flow gates present but no monitored area")
  }

  if (binaryGates.nonEmpty && monitoredAreas.isEmpty) {
    throw new ControlDevicesException("binary gates present but no monitored area, or vice-versa")
  }

  if (flowGates.exists(fg => !monitoredAreas.map(_.name).toVector.contains(fg.monitoredArea))) {
    throw new ControlDevicesException("flow gate controlled area not found in list of monitored areas")
  }

  override def clone: ControlDevices = {
    new ControlDevices(
      monitoredAreas.map(_.clone()),
      amws,
      flowGates.map(_.clone()),
      binaryGates.map(_.clone()),
      flowSeparators.map(_.clone()),
      fixedFlowSeparators
    )
  }

  def cloneModifyFlowGates[T <: Measurement, U <: Flow](f: FunctionalForm[T,U]): ControlDevices = {
    new ControlDevices(
      monitoredAreas.map(_.clone()),
      amws,
      flowGates.map(fg => fg match {
        case fgFunc: FlowGateFunctional[_, _] => {fgFunc.cloneModifyFunctionForm(f)}
        case fg: FlowGate => {fg.clone()}
      }),
      binaryGates.map(_.clone()),
      flowSeparators.map(_.clone()),
      fixedFlowSeparators
    )
  }

  def cloneModifyFlowSeparators[T <: Measurement, U <: SeparatorPositionFraction](f: FunctionalForm[T, U]): ControlDevices = {
    new ControlDevices(
      monitoredAreas.map(_.clone()),
      amws,
      flowGates.map(_.clone()),
      binaryGates.map(_.clone()),
      flowSeparators.map(_.cloneChangeFunction(f)),
      fixedFlowSeparators
    )
  }
}
