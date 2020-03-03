package hubmodel.control.amw

import hubmodel.{Position, Velocity}
import hubmodel.supply.continuous.{SINGLELINE, Wall}
import hubmodel.supply.graph.MyEdge
import tools.cells.{Circle, Vertex}

/** Implementation of moving walkways as an edge. This will be used for the route choice aspects.
  *
  * @param startVertex vertex at origin
  * @param endVertex   vertex at destination
  * @param capacity    capacity of the MV
  */
class MovingWalkway(val name: String,
                    override val startVertex: Vertex,
                    override val endVertex: Vertex,
                    val width: Double,
                    val start: Position,
                    val end: Position,
                    val associatedZonesStart: Vector[Vertex],
                    val associatedZonesEnd: Vector[Vertex],
                    val droppedVertices: Vector[String],
                    val associatedConnectivity: Iterable[MyEdge],
                    val parallelFlows: Vector[Vector[Vertex]]) extends MyEdge(startVertex, endVertex) {

  // Direction in which the pedestrians will move when on the moving walkway
  private val direction: Position = (this.end - this.start).normalized

  // A positive speed makes pedetrians move from start to end a a negative speed makes pedestrians
  // move from end to start.
  private var _speed: Double = 2.0

  def speed: Double = this._speed

  def updateSpeed(s: Double): Unit = this._speed = s

  def movingSpeed: Velocity = this.direction * this._speed

  def walls: Vector[Wall] = {
    val orthDir = (this.end - this.start).orthogonal.normalized
    Vector(
      Wall(this.name+"1", this.start - orthDir * 0.5*width, this.end - orthDir * 0.5*width, SINGLELINE),
      Wall(this.name+"2", this.start + orthDir * 0.5*width, this.end + orthDir * 0.5*width, SINGLELINE)
    )
  }

  /** create a deep copy of the control device
    *
    * @return
    */
  override def deepCopy: MovingWalkway = new MovingWalkway(
    this.name,
    this.startVertex,
    this.endVertex,
    this.width,
    this.start,
    this.end,
    this.associatedZonesStart.map(_.deepCopy),
    this.associatedZonesEnd.map(_.deepCopy),
    this.droppedVertices,
    this.associatedConnectivity,
    this.parallelFlows
  )

  def deepCopyWithState: MovingWalkway = {
   val amw = new MovingWalkway(
      this.name,
      this.startVertex,
      this.endVertex,
      this.width,
      this.start,
      this.end,
      this.associatedZonesStart.map(_.deepCopy),
      this.associatedZonesEnd.map(_.deepCopy),
      this.droppedVertices,
      this.associatedConnectivity,
     this.parallelFlows
    )
    amw.updateSpeed(this.speed)
    return amw
  }

}
