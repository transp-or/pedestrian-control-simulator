package hubmodel.supply.graph

import hubmodel.input.JSONReaders.InfraGraphParser
import hubmodel.mgmt.{FlowLine, FlowSeparator}
import hubmodel.supply.Infrastructure
import hubmodel.tools.cells.{NewVertexRectangle, NewVertexRectangleModifiable, RectangularVertexTrait}
import hubmodel.{Position, _}
import myscala.math.vector.Vector2D
import play.api.libs.json.{JsError, JsSuccess, JsValue, Json}

import scala.io.BufferedSource



/** Class to read the graph specification file and process it.
  *
  * TODO: convert this class to a function
  *
  * @param graphSpecificationFile JSON file containing the graph specification
  */
class GraphReader(graphSpecificationFile: String,
                  useFlowGates: Boolean,
                  useBinarygates: Boolean,
                  useAMWs: Boolean,
                  useFlowSep: Boolean,
                  measureDensity: Boolean) extends Infrastructure {

  override val location: String = "test"
  override val subLocation: String = "test2"

  val (graph, flowGates, binaryGates, amws, flowSep, monitoredAreas): (RouteGraph, Iterable[FlowGate], Iterable[BinaryGate], Iterable[MovingWalkway], Iterable[FlowSeparator], Iterable[RectangularVertexTrait]) = {
    val source: BufferedSource = scala.io.Source.fromFile(graphSpecificationFile)
    val input: JsValue = Json.parse(try source.mkString finally source.close)

    input.validate[InfraGraphParser] match {
      case s: JsSuccess[InfraGraphParser] =>
        val v: Vector[RectangularVertexTrait] = s.get.nodes.map(n => new NewVertexRectangle(n.name, Vector2D(n.x1, n.y1), Vector2D(n.x2, n.y2), Vector2D(n.x3, n.y3), Vector2D(n.x4, n.y4)))
        val vertexMap: Map[String, RectangularVertexTrait] = v.map(v => v.name -> v).toMap
        val e: Iterable[MyEdge] = s.get.standardConnections.flatMap(c => c.conn.map(neigh => new MyEdge(vertexMap(c.node), vertexMap(neigh))))
        val fg: Iterable[FlowGate] = if (useFlowGates) {
          s.get.flowGates.map(fg => new FlowGate(vertexMap(fg.o), vertexMap(fg.d), Vector2D(fg.start_pos_x, fg.start_pos_x), Vector2D(fg.end_pos_x, fg.end_pos_y), fg.area))
        } else {
          Vector()
        }
        val mv: Iterable[MovingWalkway] = if (useAMWs) {
          s.get.movingWalkways.map(m => new MovingWalkway(vertexMap(m.o), vertexMap(m.d), 1.0))
        } else {
          Vector()
        }
        val bg: Iterable[BinaryGate] = if (useBinarygates) {
          s.get.binaryGates.map(bg => new BinaryGate(vertexMap(bg.o), vertexMap(bg.d), Vector2D(bg.start_pos_x, bg.start_pos_y), Vector2D(bg.end_pos_x, bg.end_pos_y), bg.area))
        } else {
          Vector()
        }
        val monitoredAreas: Iterable[RectangularVertexTrait] = if (measureDensity) {
          s.get.controlledAreas.map(ma => new NewVertexRectangle(ma.name, new Position(ma.x1, ma.y1), new Position(ma.x2, ma.y2), new Position(ma.x3, ma.y3), new Position(ma.x4, ma.y4)))
        } else {
          Vector()
        }
        val flowSeparators: Iterable[FlowSeparator] = if (useFlowSep) {
          s.get.flowSeparators.map(fs => new FlowSeparator(
            Vector2D(fs.x1a, fs.y1a),
            Vector2D(fs.x1b, fs.y1b),
            Vector2D(fs.x2a, fs.y2a),
            Vector2D(fs.x2b, fs.y2b),
            fs.inf_1.map(il => new FlowLine(Vector2D(il.x1, il.y1), Vector2D(il.x2, il.y2))),
            fs.inf_2.map(il => new FlowLine(Vector2D(il.x1, il.y1), Vector2D(il.x2, il.y2))),
            fs.overZone_1.map(oz => new NewVertexRectangleModifiable(
              oz.name,
              (Vector2D(oz.x1(0), oz.y1(0)), Vector2D(oz.x1(1), oz.y1(1))),
              (Vector2D(oz.x2(0), oz.y2(0)), Vector2D(oz.x2(1), oz.y2(1))),
              (Vector2D(oz.x3(0), oz.y3(0)), Vector2D(oz.x3(1), oz.y3(1))),
              (Vector2D(oz.x4(0), oz.y4(0)), Vector2D(oz.x4(1), oz.y4(1)))
            ))
          ))
        } else {
          Vector()
        }
        (new RouteGraph(v, e, fg, bg, mv, flowSeparators), fg, bg, mv, flowSeparators, monitoredAreas)
      case e: JsError => throw new Error("Error while parsing graph specification file: " + JsError.toJson(e).toString())
    }
  }
}
