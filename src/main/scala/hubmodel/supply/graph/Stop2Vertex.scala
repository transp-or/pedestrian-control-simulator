package hubmodel.supply.graph

import hubmodel.supply.{NodeID_New, StopID_New}
import hubmodel.VertexID

class Stop2Vertex(val stop2Vertices: Map[StopID_New, Vector[VertexID]], val node2Vertices: Map[NodeID_New, Vector[VertexID]])

