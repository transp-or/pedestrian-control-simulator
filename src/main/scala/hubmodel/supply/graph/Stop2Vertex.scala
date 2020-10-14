package hubmodel.supply.graph

import hubmodel.VertexID
import hubmodel.supply.StopID_New

class Stop2Vertex(val stop2Vertices: Map[StopID_New, Vector[VertexID]], val grouping4TRANSFORM: Seq[Seq[String]])

