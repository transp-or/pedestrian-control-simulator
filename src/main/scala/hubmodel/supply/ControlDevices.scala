package hubmodel.supply

import breeze.linalg.DenseVector
import hubmodel.VertexRectangle
import hubmodel.tools.ControlDevicesException
import play.api.libs.json.{JsError, JsSuccess, JsValue, Json}
import myscala.math.vector.Vector2D

import scala.io.BufferedSource

class ReadControlDevices(graph: GraphReader) {

  val (monitoredAreas, amws, flowGates, binaryGates): (Iterable[VertexRectangle], Iterable[MovingWalkway], Iterable[FlowGate], Iterable[BinaryGate]) = {
    (
      graph.monitoredAreas,
      graph.amws,
      graph.flowGates,
      graph.binaryGates)
  }

  if (flowGates.nonEmpty != monitoredAreas.nonEmpty) {
    throw new ControlDevicesException("flow gates present but no monitored area, or vice-versa")
  }

  if (binaryGates.nonEmpty != monitoredAreas.nonEmpty) {
    throw new ControlDevicesException("binary gates present but no monitored area, or vice-versa")
  }

  if (flowGates.exists(fg => !monitoredAreas.map(_.name).toVector.contains(fg.monitoredArea))) {
    throw new ControlDevicesException("flow gate controlled area not found in list of monitored areas")
  }
}
