package hubmodel.supply

import hubmodel.Position
import hubmodel.input.JSONReaders.{Connectivity_JSON, InfraGraphParser, Track2NodeMapping_JSON}
import hubmodel.mgmt.ControlDevices
import hubmodel.mgmt.flowsep.{FlowLine, FlowSeparator}
import hubmodel.ped.{PedestrianNOMAD, PedestrianNOMADWithGraph, PedestrianSim}
import hubmodel.tools.cells.{DensityMeasuredArea, Rectangle, RectangleModifiable}
import myscala.math.vector.Vector2D
import nl.tudelft.pedestrians.agents.Pedestrian
import play.api.libs.json.{JsError, JsSuccess, JsValue, Json}

import scala.io.BufferedSource
import scala.reflect.ClassTag

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
                measureDensity: Boolean): (RouteGraphParent[PedestrianNOMAD], ControlDevices) = {


    val source: BufferedSource = scala.io.Source.fromFile(graphSpecificationFile)
    val input: JsValue = Json.parse(try source.mkString finally source.close)

    input.validate[InfraGraphParser] match {
      case s: JsSuccess[InfraGraphParser] =>
        val v: Vector[Rectangle] = s.get.nodes.map(n => new Rectangle(n.name, Vector2D(n.x1, n.y1), Vector2D(n.x2, n.y2), Vector2D(n.x3, n.y3), Vector2D(n.x4, n.y4), n.OD))
        var vertexMap: Map[String, Rectangle] = v.map(v => v.name -> v).toMap

        val fg: Iterable[FlowGate] = if (useFlowGates) {
          s.get.flowGates.map(fg => fg.funcForm match {
            case Some(str) if str == "quadratic" => new FlowGateFunctional(vertexMap(fg.o), vertexMap(fg.d), Vector2D(fg.start_pos_x, fg.start_pos_y), Vector2D(fg.end_pos_x, fg.end_pos_y), fg.area, { x: Double => math.min(10.0, math.max(0.0, fg.funcParam.get(0) + fg.funcParam.get(1) * x + fg.funcParam.get(2) * x * x)) })
            case Some(str) if str == "linear" => new FlowGateFunctional(vertexMap(fg.o), vertexMap(fg.d), Vector2D(fg.start_pos_x, fg.start_pos_y), Vector2D(fg.end_pos_x, fg.end_pos_y), fg.area, { x: Double => math.min(10.0, math.max(0.0, fg.funcParam.get(0) + fg.funcParam.get(1) * x)) })
            case None => new FlowGate(vertexMap(fg.o), vertexMap(fg.d), Vector2D(fg.start_pos_x, fg.start_pos_y), Vector2D(fg.end_pos_x, fg.end_pos_y), fg.area)
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
          vertexMap = {
            vertexMap --
              s.get.flowSeparators.flatMap(_.overZone_1.collect({ case oZone if oZone.overridenZone.isDefined => oZone.overridenZone.get })) --
              s.get.flowSeparators.flatMap(_.overZone_2.collect({ case oZone if oZone.overridenZone.isDefined => oZone.overridenZone.get })) ++
              s.get.flowSeparators.flatMap(_.overZone_1.map(oz => oz.name -> new RectangleModifiable(
                oz.name,
                (Vector2D(oz.x1(0), oz.y1(0)), Vector2D(oz.x1(1), oz.y1(1))),
                (Vector2D(oz.x2(0), oz.y2(0)), Vector2D(oz.x2(1), oz.y2(1))),
                (Vector2D(oz.x3(0), oz.y3(0)), Vector2D(oz.x3(1), oz.y3(1))),
                (Vector2D(oz.x4(0), oz.y4(0)), Vector2D(oz.x4(1), oz.y4(1))),
                false
              ))).toMap ++
              s.get.flowSeparators.flatMap(_.overZone_2.map(oz => oz.name -> new RectangleModifiable(
                oz.name,
                (Vector2D(oz.x1(0), oz.y1(0)), Vector2D(oz.x1(1), oz.y1(1))),
                (Vector2D(oz.x2(0), oz.y2(0)), Vector2D(oz.x2(1), oz.y2(1))),
                (Vector2D(oz.x3(0), oz.y3(0)), Vector2D(oz.x3(1), oz.y3(1))),
                (Vector2D(oz.x4(0), oz.y4(0)), Vector2D(oz.x4(1), oz.y4(1))),
                false
              ))).toMap
          }

          s.get.flowSeparators.map(fs => {
            val oz_1: Iterable[RectangleModifiable] = fs.overZone_1.map(oz => new RectangleModifiable(
              oz.name,
              (Vector2D(oz.x1(0), oz.y1(0)), Vector2D(oz.x1(1), oz.y1(1))),
              (Vector2D(oz.x2(0), oz.y2(0)), Vector2D(oz.x2(1), oz.y2(1))),
              (Vector2D(oz.x3(0), oz.y3(0)), Vector2D(oz.x3(1), oz.y3(1))),
              (Vector2D(oz.x4(0), oz.y4(0)), Vector2D(oz.x4(1), oz.y4(1))),
              false
            ))
            val oz_2: Iterable[RectangleModifiable] = fs.overZone_2.map(oz => new RectangleModifiable(
              oz.name,
              (Vector2D(oz.x1(0), oz.y1(0)), Vector2D(oz.x1(1), oz.y1(1))),
              (Vector2D(oz.x2(0), oz.y2(0)), Vector2D(oz.x2(1), oz.y2(1))),
              (Vector2D(oz.x3(0), oz.y3(0)), Vector2D(oz.x3(1), oz.y3(1))),
              (Vector2D(oz.x4(0), oz.y4(0)), Vector2D(oz.x4(1), oz.y4(1))),
              false
            ))
            val oldZones: Iterable[String] = fs.overZone_1.collect({ case oZone if oZone.overridenZone.isDefined => oZone.overridenZone.get }) ++ fs.overZone_2.collect({ case oZone if oZone.overridenZone.isDefined => oZone.overridenZone.get })

            new FlowSeparator(
              Vector2D(fs.x1a, fs.y1a),
              Vector2D(fs.x1b, fs.y1b),
              Vector2D(fs.x2a, fs.y2a),
              Vector2D(fs.x2b, fs.y2b),
              fs.inf_1.map(il => new FlowLine(Vector2D(il.x1, il.y1), Vector2D(il.x2, il.y2))),
              fs.inf_2.map(il => new FlowLine(Vector2D(il.x1, il.y1), Vector2D(il.x2, il.y2))),
              oz_1,
              oz_2,
              fs.overConn.collect({ case c if vertexMap.contains(c.node) => c.conn.collect({ case neigh if vertexMap.contains(neigh) => new MyEdge(vertexMap(c.node), vertexMap(neigh)) }) }).flatten,
              oldZones
            )
          }
          )
        } else {
          Vector()
        }

        /**
          * Transforms the connections from JSON connectivity objects to edges used for the graph.
          * @param edges set of edges stored as JSON objects
          * @tparam T subtype of the [[MyEdge]] to return
          * @return collection of type T tedges
          */
        def connections2Edges[T <: MyEdge](edges: Iterable[Connectivity_JSON])(implicit tag: ClassTag[T]): Iterable[T] = {
          edges.flatMap(c => c.conn.map(neigh => {
            tag.runtimeClass.getConstructors()(0).newInstance(vertexMap(c.node), vertexMap(neigh)).asInstanceOf[T]
          }))
        }

        val levelChanges: Iterable[MyEdgeLevelChange] = connections2Edges[MyEdgeLevelChange](s.get.levelChanges)//.flatMap(c => c.conn.map(neigh => new MyEdgeLevelChange(vertexMap(c.node), vertexMap(neigh))))
      val baseEdgeCollection: Iterable[MyEdge] = connections2Edges[MyEdge](s.get.standardConnections) ++ levelChanges//.flatMap(c => c.conn.map(neigh => new MyEdge(vertexMap(c.node), vertexMap(neigh)))) ++ levelChanges


        // Returns the graph object and the control devices
        (
          if (s.get.alternateConnections.isEmpty) { new RouteGraph[PedestrianNOMAD](v, baseEdgeCollection, levelChanges, fg, bg, mv, flowSeparators) }
          else {
            val graphs = new RouteGraphMultiple[PedestrianNOMADWithGraph](levelChanges, fg, bg, mv, flowSeparators)
            graphs.addGraph("base", 1.0 - s.get.alternateConnections.foldLeft(0.0)((a, b) => a + b.frac), v, baseEdgeCollection)
            s.get.alternateConnections.foreach(g => {
              val edges2Remove = connections2Edges[MyEdge](g.conn2Remove).toVector
              val edges: Iterable[MyEdge] = baseEdgeCollection.filterNot(e => edges2Remove.contains(e)) ++ connections2Edges[MyEdge](g.conn2Add)
              graphs.addGraph(g.name, g.frac, v, edges)
            })
            graphs
          },
          new ControlDevices(monitoredAreas, mv, fg, bg, flowSeparators)
        )

      case e: JsError => throw new Error("Error while parsing graph specification file: " + JsError.toJson(e).toString())
    }
  }


  def readPTStop2GraphVertexMap(fileName: String): Stop2Vertex = {

    val source: BufferedSource = scala.io.Source.fromFile(fileName)
    val input: JsValue = Json.parse(try source.mkString finally source.close)

    input.validate[Track2NodeMapping_JSON] match {
      case s: JsSuccess[Track2NodeMapping_JSON] => new Stop2Vertex(s.get.Track2NodeMappingInput.map(t => StopID_New(t.track, "") -> t.nodes).toMap, s.get.WalkingZones2NodeMappingInput.map(t => NodeID_New(t.zone.toString, "") -> t.nodes).toMap)
      case e: JsError => throw new Error("Error while parsing zones to nodes mapping: " + JsError.toJson(e).toString())
    }
  }


}
