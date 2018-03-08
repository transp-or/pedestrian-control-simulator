package hubmodel.supply

import breeze.linalg.DenseVector
import hubmodel.VertexRectangle
import hubmodel.tools.ControlDevicesException
import play.api.libs.json.{JsError, JsSuccess, JsValue, Json}
import myscala.math.vector.Vector2D

import scala.io.BufferedSource

class ReadControlDevices(file: String, vertexMap: Map[String, VertexRectangle]) {
  val (monitoredAreas, amws, flowGates, binaryGates): (Vector[VertexRectangle], Vector[MovingWalkway], Vector[FlowGate], Vector[BinaryGate]) = {
    val source: BufferedSource = scala.io.Source.fromFile(file)
    val input: JsValue = Json.parse(try source.mkString finally source.close)

    input.validate[ControlElementsParser] match {
      case s: JsSuccess[ControlElementsParser] =>
        (
          s.get.criticalZones.map(z => VertexRectangle(z.name, Vector2D(z.x1, z.y1), Vector2D(z.x2, z.y2), Vector2D(z.x3, z.y3), Vector2D(z.x4, z.y4))),
          s.get.amws.map(m => new MovingWalkway(vertexMap(m.o), vertexMap(m.d), 1.0)),
          s.get.flowGates.map(fg => {println(fg.o, fg.d);new FlowGate(vertexMap(fg.o), vertexMap(fg.d), DenseVector(fg.start_pos_x, fg.start_pos_y), DenseVector(fg.end_pos_x, fg.end_pos_y), fg.area)}),
          s.get.binaryGates.map(bg => new BinaryGate(vertexMap(bg.o), vertexMap(bg.d), DenseVector(bg.s_x, bg.s_y), DenseVector(bg.e_x, bg.e_y), bg.area))
        )
      case e: JsError => throw new Error("Error while parsing graph specification file for control elements: " + JsError.toJson(e).toString())
    }
  }

  private val vertexMapExtened: Map[String, VertexRectangle] = vertexMap ++ monitoredAreas.map(ma => ma.name -> ma)


  if (flowGates.nonEmpty != monitoredAreas.nonEmpty) {
    throw new ControlDevicesException("flow gates present but no monitored area, or vice-versa")
  }

  if (binaryGates.nonEmpty != monitoredAreas.nonEmpty) {
    throw new ControlDevicesException("binary gates present but no monitored area, or vice-versa")
  }

  if (flowGates.exists(fg => !monitoredAreas.map(_.name).contains(fg.monitoredArea))) {
    throw new ControlDevicesException("flow gate controlled area not found in list of monitored areas")
  }
}
