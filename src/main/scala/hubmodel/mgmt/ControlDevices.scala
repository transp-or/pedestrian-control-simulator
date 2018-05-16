package hubmodel.mgmt

import hubmodel.supply.graph._
import hubmodel.tools.ControlDevicesException
import hubmodel.tools.cells.Rectangle


class ControlDevices(val monitoredAreas: Iterable[Rectangle],
                     val amws: Iterable[MovingWalkway],
                     val flowGates: Iterable[FlowGate],
                     val binaryGates: Iterable[BinaryGate],
                     val flowSeparators: Iterable[FlowSeparator]) {

  if (flowGates.nonEmpty && monitoredAreas.isEmpty) {
    throw new ControlDevicesException("flow gates present but no monitored area")
  }

  if (binaryGates.nonEmpty && monitoredAreas.isEmpty) {
    throw new ControlDevicesException("binary gates present but no monitored area, or vice-versa")
  }

  if (flowGates.exists(fg => !monitoredAreas.map(_.name).toVector.contains(fg.monitoredArea))) {
    throw new ControlDevicesException("flow gate controlled area not found in list of monitored areas")
  }
}
