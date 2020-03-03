package hubmodel.supply

import hubmodel.Position
import hubmodel.io.input.JSONReaders.{Connectivity_JSON, InfraGraphParser, Track2NodeMapping_JSON}
import hubmodel.control._
import hubmodel.control.amw.MovingWalkway
import hubmodel.control.flowgate.{BinaryGate, FlowGate, FlowGateFunctional}
import hubmodel.control.flowsep._
import hubmodel.ped.PedestrianNOMAD
import tools.cells.{Circle, DensityMeasuredArea, Rectangle, RectangleModifiable, Vertex}
import myscala.math.vector.Vector2D
import play.api.libs.json.{JsError, JsSuccess, JsValue, Json}

import scala.collection.mutable.ArrayBuffer
import scala.io.BufferedSource
import scala.reflect.ClassTag
import scala.util.{Failure, Success, Try}

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
                                      fixedFlowSep: Boolean,
                                      measureDensity: Boolean,
                                      useAlternatGraphs: Boolean)(implicit tagT: ClassTag[PedestrianNOMAD]): (GraphContainer, ControlDevices) = {


    val source: BufferedSource = scala.io.Source.fromFile(graphSpecificationFile)
    val input: JsValue = Json.parse(try source.mkString finally source.close)

    input.validate[InfraGraphParser] match {
      case s: JsSuccess[InfraGraphParser] => {
        val v: Vector[Rectangle] = s.get.nodes.map(n => new Rectangle(n.name, Vector2D(n.x1, n.y1), Vector2D(n.x2, n.y2), Vector2D(n.x3, n.y3), Vector2D(n.x4, n.y4), n.OD, n.rate))
        val vertexMapReader: collection.mutable.Map[String, Vertex] = collection.mutable.Map() ++ v.map(v => v.name -> v)
        val edgesFromVertexToRemove: ArrayBuffer[String] = ArrayBuffer()

        val fg: Iterable[FlowGate] = if (useFlowGates) {
          s.get.flowGates.map(fg => fg.funcForm match {
            case Some(str) if str == "quadratic" => new FlowGateFunctional(vertexMapReader(fg.o), vertexMapReader(fg.d), Vector2D(fg.start_pos_x, fg.start_pos_y), Vector2D(fg.end_pos_x, fg.end_pos_y), fg.area, {
              FunctionalFormGating((x: Density) => Flow(math.max(0.0, fg.funcParam.get(0) + fg.funcParam.get(1) * x.d + fg.funcParam.get(2) * x.d * x.d)))
            })
            case Some(str) if str == "linear" => new FlowGateFunctional(vertexMapReader(fg.o), vertexMapReader(fg.d), Vector2D(fg.start_pos_x, fg.start_pos_y), Vector2D(fg.end_pos_x, fg.end_pos_y), fg.area, {
              FunctionalFormGating((x: Density) => Flow(math.max(0.0, fg.funcParam.get(0) + fg.funcParam.get(1) * x.d)))
            })
            case None => new FlowGate(vertexMapReader(fg.o), vertexMapReader(fg.d), Vector2D(fg.start_pos_x, fg.start_pos_y), Vector2D(fg.end_pos_x, fg.end_pos_y), fg.area)
          })
        } else {
          Vector()
        }
        val mv: Iterable[MovingWalkway] = if (useAMWs) {

          // remove overriden vertices from map
          vertexMapReader --= s.get.movingWalkways.flatMap(_.overriden_zones_1.collect({ case oZone if oZone.overridenZone.isDefined => oZone.overridenZone.get }))
          vertexMapReader --= s.get.movingWalkways.flatMap(_.overriden_zones_2.collect({ case oZone if oZone.overridenZone.isDefined => oZone.overridenZone.get }))

          s.get.movingWalkways.map(m => {

            edgesFromVertexToRemove.appendAll(m.overriden_connections.map(_.node))

            // create new zones from overriden zones
            val oz_1 = m.overriden_zones_1.map(oz => new Rectangle(
              oz.name,
              new Position(oz.x1, oz.y1),
              new Position(oz.x2, oz.y2),
              new Position(oz.x3, oz.y3),
              new Position(oz.x4, oz.y4),
              oz.isOD,
              oz.maxRate
            ))

            val oz_2 = m.overriden_zones_2.map(oz => new Rectangle(
              oz.name,
              new Position(oz.x1, oz.y1),
              new Position(oz.x2, oz.y2),
              new Position(oz.x3, oz.y3),
              new Position(oz.x4, oz.y4),
              oz.isOD,
              oz.maxRate
            ))


            // create start and end points
            val start = new Position(m.x1, m.y1)
            val end = new Position(m.x2, m.y2)

            // create start and end vertices
            val startCircle: Circle = new Circle( m.name + "1",
                start + (start - end).normalized * 0.5 * (m.width - 0.2),
                0.5 * (m.width - 0.2),
                false,
                None
              )

            val endCircle: Circle =  new Circle(                m.name + "2",
                end + (end - start).normalized * 0.5*(m.width-0.2),
                0.5*(m.width-0.2),
                false,
                None
              )

            // collect zones which have been removed
            val oldZones: Vector[String] = m.overriden_zones_1.flatMap(oz => oz.overridenZone) ++ m.overriden_zones_2.flatMap(oz => oz.overridenZone)

            // update the map of vertices with the new ones
            vertexMapReader ++= oz_1.map(z => z.name -> z)
            vertexMapReader ++= oz_2.map(z => z.name -> z)
            vertexMapReader ++= Vector((startCircle.name, startCircle), (endCircle.name, endCircle))

            val newConnections = m.overriden_connections.collect({ case c if vertexMapReader.contains(c.node) => c.conn.collect({ case neigh if vertexMapReader.contains(neigh) => new MyEdge(vertexMapReader(c.node), vertexMapReader(neigh)) }) }).flatten

            // create AMW
            new MovingWalkway(m.name, startCircle, endCircle, m.width, start, end, oz_1, oz_2, oldZones, newConnections, m.parallel_flows.map(r => r.map(v => vertexMapReader(v))))
          })

        } else {
          Vector()
        }
        val bg: Iterable[BinaryGate] = if (useBinarygates) {
          s.get.binaryGates.map(bg => new BinaryGate(vertexMapReader(bg.o), vertexMapReader(bg.d), Vector2D(bg.start_pos_x, bg.start_pos_y), Vector2D(bg.end_pos_x, bg.end_pos_y), bg.area))
        } else {
          Vector()
        }
        val monitoredAreas: Iterable[DensityMeasuredArea] = if (measureDensity) {
          s.get.controlledAreas.map(ma => new DensityMeasuredArea(ma.name, new Position(ma.x1, ma.y1), new Position(ma.x2, ma.y2), new Position(ma.x3, ma.y3), new Position(ma.x4, ma.y4), ma.targetDensity))
        } else {
          Vector()
        }
        val flowSeparators: Iterable[FlowSeparator[_, _]] = if (useFlowSep) {

          // updates the vertex map to remove overriden nodes and add the new ones linked to the flow separators

          vertexMapReader --= s.get.flowSeparators.flatMap(_.overZone_1.collect({ case oZone if oZone.overridenZone.isDefined => oZone.overridenZone.get }))
          vertexMapReader --= s.get.flowSeparators.flatMap(_.overZone_2.collect({ case oZone if oZone.overridenZone.isDefined => oZone.overridenZone.get }))


          s.get.flowSeparators.map(fs => {

            edgesFromVertexToRemove.appendAll(fs.overConn.map(_.node))


            val oz_1: Iterable[RectangleModifiable] = fs.overZone_1.map(oz => new RectangleModifiable(
              oz.name,
              (Vector2D(oz.x1(0), oz.y1(0)), Vector2D(oz.x1(1), oz.y1(1))),
              (Vector2D(oz.x2(0), oz.y2(0)), Vector2D(oz.x2(1), oz.y2(1))),
              (Vector2D(oz.x3(0), oz.y3(0)), Vector2D(oz.x3(1), oz.y3(1))),
              (Vector2D(oz.x4(0), oz.y4(0)), Vector2D(oz.x4(1), oz.y4(1))),
              oz.isOD,
              oz.maxRate
            ))
            val oz_2: Iterable[RectangleModifiable] = fs.overZone_2.map(oz => new RectangleModifiable(
              oz.name,
              (Vector2D(oz.x1(0), oz.y1(0)), Vector2D(oz.x1(1), oz.y1(1))),
              (Vector2D(oz.x2(0), oz.y2(0)), Vector2D(oz.x2(1), oz.y2(1))),
              (Vector2D(oz.x3(0), oz.y3(0)), Vector2D(oz.x3(1), oz.y3(1))),
              (Vector2D(oz.x4(0), oz.y4(0)), Vector2D(oz.x4(1), oz.y4(1))),
              oz.isOD,
              oz.maxRate
            ))
            val oldZones: Iterable[String] = fs.overZone_1.collect({ case oZone if oZone.overridenZone.isDefined => oZone.overridenZone.get }) ++ fs.overZone_2.collect({ case oZone if oZone.overridenZone.isDefined => oZone.overridenZone.get })

            vertexMapReader ++= oz_1.map(z => z.name -> z)
            vertexMapReader ++= oz_2.map(z => z.name -> z)


            new FlowSeparator(
              Vector2D(fs.x1a, fs.y1a),
              Vector2D(fs.x1b, fs.y1b),
              Vector2D(fs.x2a, fs.y2a),
              Vector2D(fs.x2b, fs.y2b),
              fs.inf_1.map(il => new FlowLine(Vector2D(il.x1, il.y1), Vector2D(il.x2, il.y2))),
              fs.inf_2.map(il => new FlowLine(Vector2D(il.x1, il.y1), Vector2D(il.x2, il.y2))),
              oz_1,
              oz_2,
              fs.overConn.collect({ case c if vertexMapReader.contains(c.node) => c.conn.collect({ case neigh if vertexMapReader.contains(neigh) => new MyEdge(vertexMapReader(c.node), vertexMapReader(neigh)) }) }).flatten,
              oldZones,
              FunctionalFormFlowSeparator((bf: BidirectionalFlow) => SeparatorPositionFraction(bf.f2 / (bf.f1 + bf.f2)))
            )
          }
          )
        } else {
          Vector()
        }

        val flowSepParameters: Vector[FlowSeparatorParameters[_, _]] = {
          s.get.flowSeparators.map(fs => {

            val oz_1: Iterable[RectangleModifiableParameters] = fs.overZone_1.map(oz => RectangleModifiableParameters(
              oz.name,
              (Vector2D(oz.x1(0), oz.y1(0)), Vector2D(oz.x1(1), oz.y1(1))),
              (Vector2D(oz.x2(0), oz.y2(0)), Vector2D(oz.x2(1), oz.y2(1))),
              (Vector2D(oz.x3(0), oz.y3(0)), Vector2D(oz.x3(1), oz.y3(1))),
              (Vector2D(oz.x4(0), oz.y4(0)), Vector2D(oz.x4(1), oz.y4(1))),
              oz.isOD,
              oz.maxRate
            ))
            val oz_2: Iterable[RectangleModifiableParameters] = fs.overZone_2.map(oz => RectangleModifiableParameters(
              oz.name,
              (Vector2D(oz.x1(0), oz.y1(0)), Vector2D(oz.x1(1), oz.y1(1))),
              (Vector2D(oz.x2(0), oz.y2(0)), Vector2D(oz.x2(1), oz.y2(1))),
              (Vector2D(oz.x3(0), oz.y3(0)), Vector2D(oz.x3(1), oz.y3(1))),
              (Vector2D(oz.x4(0), oz.y4(0)), Vector2D(oz.x4(1), oz.y4(1))),
              oz.isOD,
              oz.maxRate
            ))

            FlowSeparatorParameters(
              Vector2D(fs.x1a, fs.y1a),
              Vector2D(fs.x1b, fs.y1b),
              Vector2D(fs.x2a, fs.y2a),
              Vector2D(fs.x2b, fs.y2b),
              fs.inf_1.map(il => FlowLineParameters(Vector2D(il.x1, il.y1), Vector2D(il.x2, il.y2), 0)),
              fs.inf_2.map(il => FlowLineParameters(Vector2D(il.x1, il.y1), Vector2D(il.x2, il.y2), 0)),
              oz_1,
              oz_2,
              fs.overConn.flatMap(conn => conn.conn.map(c => (conn.node, c))),
              FunctionalFormFlowSeparator((bf: BidirectionalFlow) => SeparatorPositionFraction(bf.f2 / (bf.f1 + bf.f2)))
            )
          }
          )
        }


        /**
          * Transforms the connections from JSON connectivity objects to edges used for the graph.
          *
          * @param edges set of edges stored as JSON objects
          * @tparam U subtype of the [[MyEdge]] to return
          * @return collection of type T tedges
          */

        def connections2Edges[U <: MyEdge](edges: Iterable[Connectivity_JSON])(implicit tag: ClassTag[U]): Set[U] = {
          edges
            .flatMap(c => c.conn
              .collect({ case neigh if vertexMapReader.keySet.contains(c.node) && vertexMapReader.keySet.contains(neigh) =>
                tag.runtimeClass.getConstructors()(0).newInstance(vertexMapReader(c.node), vertexMapReader(neigh)).asInstanceOf[U]
              })
            ).toSet
        }

        val levelChanges: Vector[MyEdgeLevelChange] = connections2Edges[MyEdgeLevelChange](s.get.levelChanges).toVector
        //.flatMap(c => c.conn.map(neigh => new MyEdgeLevelChange(vertexMap(c.node), vertexMap(neigh))))
        val baseEdgeCollection: Vector[MyEdge] = (connections2Edges[MyEdge](s.get.standardConnections) ++ levelChanges).toVector.filterNot(e => edgesFromVertexToRemove.contains(e.startVertex.name)) //.flatMap(c => c.conn.map(neigh => new MyEdge(vertexMap(c.node), vertexMap(neigh)))) ++ levelChanges


        val graph: Try[GraphContainer] = Try(
          if (!useAlternatGraphs && s.get.alternateConnections.isEmpty) {
            //tagT.runtimeClass.getConstructors()(0).newInstance(v, baseEdgeCollection, levelChanges, fg, bg, mv, flowSeparators).asInstanceOf[T]
            new SingleGraph(v, baseEdgeCollection, levelChanges, s.get.destinationEquivalencies.map(r => (r.name, r.zones)), fg, bg, mv, flowSeparators)
          } else if (useAlternatGraphs && s.get.alternateConnections.nonEmpty) {
            val graphs = new MultipleGraph(fg, bg, mv, flowSeparators)
            graphs.addGraph("reference", 1.0 - s.get.alternateConnections.foldLeft(0.0)((a, b) => a + b.frac), v, baseEdgeCollection, Set(), Set(), levelChanges, s.get.destinationEquivalencies.map(r => (r.name, r.zones)))
            s.get.alternateConnections.foreach(g => {
              graphs.addGraph(g.name, g.frac, v, baseEdgeCollection, connections2Edges[MyEdge](g.conn2Add), connections2Edges[MyEdge](g.conn2Remove), levelChanges, s.get.destinationEquivalencies.map(r => (r.name, r.zones)))
            })
            graphs
          } else {
            throw new IllegalArgumentException("Error processing graphs ! Requested alternate graphs but alternate_graphs JSON is empty !")
          })

        // Returns the graph object and the control devices
        (
          graph match {
            case Success(g) => g
            case Failure(f) => {
              new SingleGraph(v, baseEdgeCollection, levelChanges, s.get.destinationEquivalencies.map(r => (r.name, r.zones)), fg, bg, mv, flowSeparators)
            }
          }
          ,
          new ControlDevices(monitoredAreas, mv, fg, bg, flowSeparators, fixedFlowSep, Some(flowSepParameters))
        )
      }
      case e: JsError => throw new Error("Error while parsing graph specification file: " + JsError.toJson(e).toString())
    }
  }


  def readPTStop2GraphVertexMap(fileName: String): Stop2Vertex = {

    val source: BufferedSource = scala.io.Source.fromFile(fileName)
    val input: JsValue = Json.parse(try source.mkString finally source.close)

    input.validate[Track2NodeMapping_JSON] match {
      case s: JsSuccess[Track2NodeMapping_JSON] => new Stop2Vertex(s.get.Track2NodeMappingInput.map(t => StopID_New(t.stop, "") -> t.nodes).toMap, s.get.grouping4TRANSFORM)
      case e: JsError => throw new Error("Error while parsing zones to nodes mapping: " + JsError.toJson(e).toString())
    }
  }


}
