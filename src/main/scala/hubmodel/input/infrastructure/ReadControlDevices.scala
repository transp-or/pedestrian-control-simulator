package hubmodel.input.infrastructure

import breeze.linalg.DenseVector
import hubmodel.{IncompatibleControlDevicesException, VertexCell}
import play.api.libs.json.{JsError, JsSuccess, JsValue, Json}

import scala.io.BufferedSource

class ReadControlDevices(file: String, vertexMap: Map[String, VertexCell]) {

  val (monitoredAreas, amws, flowGates, binaryGates): (Vector[VertexCell], Vector[MovingWalkway], Vector[FlowGate], Vector[BinaryGate]) = {
    val source: BufferedSource = scala.io.Source.fromFile(file)
    val input: JsValue = Json.parse(try source.mkString finally source.close)

    input.validate[ControlElementsParser] match {
      case s: JsSuccess[ControlElementsParser] =>
        (
          s.get.criticalZones.map(z => VertexCell(z.name, DenseVector(z.x1, z.y1), DenseVector(z.x2, z.y2), DenseVector(z.x3, z.y3), DenseVector(z.x4, z.y4))),
          s.get.amws.map(m => new MovingWalkway(vertexMap(m.o), vertexMap(m.d), 1.0)),
          s.get.flowGates.map(fg => new FlowGate(vertexMap(fg.o), vertexMap(fg.d), DenseVector(fg.start_pos_x, fg.start_pos_y), DenseVector(fg.end_pos_x, fg.end_pos_y), fg.area)),
          s.get.binaryGates.map(bg => new BinaryGate(vertexMap(bg.o), vertexMap(bg.d), DenseVector(bg.s_x, bg.s_y), DenseVector(bg.e_x, bg.e_y), bg.area))
        )
      case e: JsError => throw new Error("Error while parsing graph specification file: " + JsError.toJson(e).toString())
    }
  }

  private val vertexMapExtened: Map[String, VertexCell] = vertexMap ++ monitoredAreas.map(ma => ma.name -> ma)


  if (flowGates.nonEmpty != monitoredAreas.nonEmpty) {
    throw new IncompatibleControlDevicesException("flow gates present but no monitored area, or vice-versa")
  }

  if (binaryGates.nonEmpty != monitoredAreas.nonEmpty) {
    throw new IncompatibleControlDevicesException("binary gates present but no monitored area, or vice-versa")
  }

  if (flowGates.map(fg => monitoredAreas.find(_ == fg.monitoredArea)).contains(None)) {
    throw new IncompatibleControlDevicesException("flow gate controlled area not found in list of monitored areas")
  }
}
