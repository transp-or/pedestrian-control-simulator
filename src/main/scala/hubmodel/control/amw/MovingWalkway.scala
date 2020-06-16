package hubmodel.control.amw

import hubmodel.Position
import hubmodel.ped.PedestrianNOMAD
import hubmodel.supply.graph.MyEdge
import tools.cells.{DensityMeasuredArea, Vertex}

class MovingWalkway(name: String,
                    firstVertex: Vertex,
                    secondVertex: Vertex,
                    width: Double,
                    start: Position,
                    end: Position,
                    associatedZonesStart: Vector[Vertex],
                    associatedZonesEnd: Vector[Vertex],
                    droppedVertices: Vector[String],
                    associatedConnectivity: Iterable[MyEdge],
                    parallelFlows: Vector[Vector[Vertex]],
                    val criticalAreaStart: Iterable[DensityMeasuredArea],
                    val criticalAreaEnd: Iterable[DensityMeasuredArea]) extends MovingWalkwayAbstract(name, firstVertex, secondVertex, width, start, end, associatedZonesStart, associatedZonesEnd, droppedVertices, associatedConnectivity, parallelFlows) {


  /** create a deep copy of the control device
    *
    * @return
    */
  override def deepCopy: MovingWalkway = new MovingWalkway(this.name, this.firstVertex, this.secondVertex, this.width, this.start, this.end, this.associatedZonesStart.map(_.deepCopy), this.associatedZonesEnd.map(_.deepCopy), this.droppedVertices, this.associatedConnectivity.map(_.deepCopy), this.parallelFlows, this.criticalAreaStart, this.criticalAreaEnd)

  override def deepCopyWithState(pop: Iterable[PedestrianNOMAD]): MovingWalkway = {
    val amw = new MovingWalkway(this.name, this.firstVertex, this.secondVertex, this.width, this.start, this.end, this.associatedZonesStart.map(_.deepCopy), this.associatedZonesEnd.map(_.deepCopy), this.droppedVertices, this.associatedConnectivity.map(_.deepCopy), this.parallelFlows, this.criticalAreaStart, this.criticalAreaEnd)
    amw._previousSpeed = this._previousSpeed
    amw._nextSpeed = this._nextSpeed
    amw.accDirection = this.accDirection
    amw.accStartTime = this.accStartTime
    amw.accEndTime = this.accEndTime
    amw.setControlPolicy(this.controlPolicy.toVector, if (this.closeTimes.nonEmpty) {
      Some(MovingWalkwayControlEvents(this.name, this.closeTimes.toVector, this.openTimes.toVector))
    } else {
      None
    })
    amw.pedestriansOnAMW.addAll(pop.filter(p => p.isInsideAMW.isDefined && p.isInsideAMW.get == amw.name).map(_.ID))
    return amw
  }

}

