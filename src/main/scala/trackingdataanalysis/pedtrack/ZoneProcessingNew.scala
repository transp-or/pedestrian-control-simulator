package trackingdataanalysis.pedtrack

import hubmodel.Position
import tools.cells.{Rectangle, Vertex}
import hubmodel.supply.graph.readGraph

import scala.annotation.tailrec

class ZoneProcessingNew(val vertices: Map[String, Vertex]) {

  // Collection of Vertices read from the file.

  /** Returns the first zone in which a point is located
    *
    * @param pos      point to find owernship
    * @param vertices list of zones
    * @return Int naming the zone, -1 if none found
    */
  @tailrec private def findZoneOwnership(pos: Position, vertices: Iterable[Vertex]): Option[Vertex] = {
    if (vertices.isEmpty) {None}
    else if (vertices.head.isInside(pos, false)) {Some(vertices.head)}
    else {findZoneOwnership(pos, vertices.tail)}
  }

  /** Returns the first zone in which the position lies. If no zone is found, returns None.
    *
    * @param pos point to place in zone
    * @return [[Vertex]] where the point lies
    */
  def findZone(pos: Position): Option[Vertex] = {
    findZoneOwnership(pos, this.vertices.values)
  }

  def this(zoneFile: String) {
    this(readGraph(zoneFile, false, false, false, false, false, false, false, ("static", "flow"), (0, 0))._1.vertexMapNew)
  }
}
