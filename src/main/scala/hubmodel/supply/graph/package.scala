package hubmodel.supply

import hubmodel.Position
import hubmodel.input.JSONReaders.{InfraGraphParser, Track2NodeMapping_JSON}
import hubmodel.mgmt.ControlDevices
import hubmodel.mgmt.flowsep.{FlowLine, FlowSeparator}
import hubmodel.tools.cells.{DensityMeasuredArea, Rectangle, RectangleModifiable}
import myscala.math.vector.Vector2D
import play.api.libs.json.{JsError, JsSuccess, JsValue, Json}

import scala.io.BufferedSource

package object graph {


  /** Class to read the graph specification file and process it.
    *
    * TODO: convert this class to a function
    *
    * @param graphSpecificationFile JSON file containing the graph specification
    */
  def readGraph(graphSpecificationFile: String,
                useFlowGates: Boolean,
                useBinarygates: Boolean,
                useAMWs: Boolean,
                useFlowSep: Boolean,
                measureDensity: Boolean): (RouteGraph, ControlDevices) = {

    val source: BufferedSource = scala.io.Source.fromFile(graphSpecificationFile)
    val input: JsValue = Json.parse(try source.mkString finally source.close)

    input.validate[InfraGraphParser] match {
      case s: JsSuccess[InfraGraphParser] =>
        val v: Vector[Rectangle] = s.get.nodes.map(n => new Rectangle(n.name, Vector2D(n.x1, n.y1), Vector2D(n.x2, n.y2), Vector2D(n.x3, n.y3), Vector2D(n.x4, n.y4)))
        val vertexMap: Map[String, Rectangle] = v.map(v => v.name -> v).toMap
        val e: Iterable[MyEdge] = s.get.standardConnections.flatMap(c => c.conn.map(neigh => new MyEdge(vertexMap(c.node), vertexMap(neigh))))
        val fg: Iterable[FlowGate] = if (useFlowGates) {
          s.get.flowGates.map(fg => fg.funcForm match {
            case Some(str) if str == "quadratic" => new FlowGateFunctional(vertexMap(fg.o), vertexMap(fg.d), Vector2D(fg.start_pos_x, fg.start_pos_y), Vector2D(fg.end_pos_x, fg.end_pos_y), fg.area, {x: Double => math.min(10.0, math.max(0.0, fg.funcParam.get(0) + fg.funcParam.get(1)*x  + fg.funcParam.get(2)*x*x))} )
            case Some(str) if str == "linear" => new FlowGateFunctional(vertexMap(fg.o), vertexMap(fg.d), Vector2D(fg.start_pos_x, fg.start_pos_y), Vector2D(fg.end_pos_x, fg.end_pos_y), fg.area, {x: Double => math.min(10.0, math.max(0.0, fg.funcParam.get(0) + fg.funcParam.get(1)*x))} )
            case None  => new FlowGate(vertexMap(fg.o), vertexMap(fg.d), Vector2D(fg.start_pos_x, fg.start_pos_y), Vector2D(fg.end_pos_x, fg.end_pos_y), fg.area)
          })
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
        val monitoredAreas: Iterable[DensityMeasuredArea] = if (measureDensity) {
          s.get.controlledAreas.map(ma => new DensityMeasuredArea(ma.name, new Position(ma.x1, ma.y1), new Position(ma.x2, ma.y2), new Position(ma.x3, ma.y3), new Position(ma.x4, ma.y4), ma.targetDensity))
        } else {
          Vector()
        }
        val flowSeparators: Iterable[FlowSeparator] = if (useFlowSep) {

          // updates the vertex map to remove overriden nodes and add the new ones linked to the flow separators
          val vertexMapUpdated: Map[String, Rectangle] = {
            vertexMap --
              s.get.flowSeparators.flatMap(_.overZone_1.collect({ case oZone if oZone.overridenZone.isDefined => oZone.overridenZone.get })) --
              s.get.flowSeparators.flatMap(_.overZone_2.collect({ case oZone if oZone.overridenZone.isDefined => oZone.overridenZone.get })) ++
              s.get.flowSeparators.flatMap(_.overZone_1.map(oz => oz.name -> new RectangleModifiable(
                oz.name,
                (Vector2D(oz.x1(0), oz.y1(0)), Vector2D(oz.x1(1), oz.y1(1))),
                (Vector2D(oz.x2(0), oz.y2(0)), Vector2D(oz.x2(1), oz.y2(1))),
                (Vector2D(oz.x3(0), oz.y3(0)), Vector2D(oz.x3(1), oz.y3(1))),
                (Vector2D(oz.x4(0), oz.y4(0)), Vector2D(oz.x4(1), oz.y4(1)))
              ))).toMap ++
              s.get.flowSeparators.flatMap(_.overZone_2.map(oz => oz.name -> new RectangleModifiable(
                oz.name,
                (Vector2D(oz.x1(0), oz.y1(0)), Vector2D(oz.x1(1), oz.y1(1))),
                (Vector2D(oz.x2(0), oz.y2(0)), Vector2D(oz.x2(1), oz.y2(1))),
                (Vector2D(oz.x3(0), oz.y3(0)), Vector2D(oz.x3(1), oz.y3(1))),
                (Vector2D(oz.x4(0), oz.y4(0)), Vector2D(oz.x4(1), oz.y4(1)))
              ))).toMap
          }

          s.get.flowSeparators.map(fs => {
            val oz_1: Iterable[RectangleModifiable] = fs.overZone_1.map(oz => new RectangleModifiable(
              oz.name,
              (Vector2D(oz.x1(0), oz.y1(0)), Vector2D(oz.x1(1), oz.y1(1))),
              (Vector2D(oz.x2(0), oz.y2(0)), Vector2D(oz.x2(1), oz.y2(1))),
              (Vector2D(oz.x3(0), oz.y3(0)), Vector2D(oz.x3(1), oz.y3(1))),
              (Vector2D(oz.x4(0), oz.y4(0)), Vector2D(oz.x4(1), oz.y4(1)))
            ))
            val oz_2: Iterable[RectangleModifiable] = fs.overZone_2.map(oz => new RectangleModifiable(
              oz.name,
              (Vector2D(oz.x1(0), oz.y1(0)), Vector2D(oz.x1(1), oz.y1(1))),
              (Vector2D(oz.x2(0), oz.y2(0)), Vector2D(oz.x2(1), oz.y2(1))),
              (Vector2D(oz.x3(0), oz.y3(0)), Vector2D(oz.x3(1), oz.y3(1))),
              (Vector2D(oz.x4(0), oz.y4(0)), Vector2D(oz.x4(1), oz.y4(1)))
            ))
            val oldZones: Iterable[String] = fs.overZone_1.collect({case oZone if oZone.overridenZone.isDefined => oZone.overridenZone.get}) ++ fs.overZone_2.collect({case oZone if oZone.overridenZone.isDefined => oZone.overridenZone.get})

            new FlowSeparator(
              Vector2D(fs.x1a, fs.y1a),
              Vector2D(fs.x1b, fs.y1b),
              Vector2D(fs.x2a, fs.y2a),
              Vector2D(fs.x2b, fs.y2b),
              fs.inf_1.map(il => new FlowLine(Vector2D(il.x1, il.y1), Vector2D(il.x2, il.y2))),
              fs.inf_2.map(il => new FlowLine(Vector2D(il.x1, il.y1), Vector2D(il.x2, il.y2))),
              oz_1,
              oz_2,
              fs.overConn.collect({ case c if vertexMapUpdated.contains(c.node) => c.conn.collect({ case neigh if vertexMapUpdated.contains(neigh) =>  new MyEdge(vertexMapUpdated(c.node), vertexMapUpdated(neigh)) }) }).flatten,
              oldZones
            )
          }
          )
        } else {
          Vector()
        }
        (new RouteGraph(v, e, fg, bg, mv, flowSeparators), new ControlDevices(monitoredAreas, mv, fg, bg, flowSeparators))
      case e: JsError => throw new Error("Error while parsing graph specification file: " + JsError.toJson(e).toString())
    }
  }


  def readStop2Vertex(fileName: String): Stop2Vertex = {

    val source: BufferedSource = scala.io.Source.fromFile(fileName)
    val input: JsValue = Json.parse(try source.mkString finally source.close)

    input.validate[Track2NodeMapping_JSON] match {
      case s: JsSuccess[Track2NodeMapping_JSON] => new Stop2Vertex(s.get.Track2NodeMappingInput.map(t => StopID_New(t.track, "") -> t.nodes).toMap, s.get.WalkingZones2NodeMappingInput.map(t => NodeID_New(t.zone.toString, "") -> t.nodes).toMap)
      case e: JsError => throw new Error("Error while parsing zones to nodes mapping: " + JsError.toJson(e).toString())
    }
  }


}
