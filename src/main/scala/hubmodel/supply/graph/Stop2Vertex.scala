package hubmodel.supply.graph

import hubmodel.VertexID
import hubmodel.supply.{NodeID_New, StopID_New}

class Stop2Vertex(val stop2Vertices: Map[StopID_New, Vector[VertexID]])

