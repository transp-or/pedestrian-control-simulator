package hubmodel.mgmt

import hubmodel.VertexRectangle
import hubmodel.supply.graph._
import hubmodel.tools.ControlDevicesException


class ControlDevices(graph: GraphReader) {

  val (monitoredAreas, amws, flowGates, binaryGates): (Iterable[VertexRectangle], Iterable[MovingWalkway], Iterable[FlowGate], Iterable[BinaryGate]) = {
    (
      graph.monitoredAreas,
      graph.amws,
      graph.flowGates,
      graph.binaryGates)
  }

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
