package hubmodel.supply.graph

import hubmodel.generateUUID
import org.jgrapht.graph.DefaultWeightedEdge
import tools.Time
import tools.cells.Vertex

/** Representation of an edge used in the graph structure. Can be used as parent for advanced edges with gates.
  *
  * @param startVertex vertex at origin
  * @param endVertex   vertex at destination
  */
class MyEdge(val startVertex: Vertex, val endVertex: Vertex) extends DefaultWeightedEdge {

  // ID of the edge
  val ID: String = generateUUID

  // distance between vertices in straight line.
  val length: Double = (startVertex.center - endVertex.center).norm

  // average walking speed
  private val averageWalkingSpeed: Double = 1.34

  // Cost of the edge in seconds (i.e. travel time)
  private var _cost: Double = length / averageWalkingSpeed

  // History of the edge costs
  private val _costHistory: collection.mutable.ArrayBuffer[(Time, Double)] = collection.mutable.ArrayBuffer()

  def costHistory: Vector[(Time,Double)] = this._costHistory.toVector

  // accessor for the cost
  def cost: Double = _cost

  // setter for the cost.
  def updateCost(t: Time, v: Double): Unit = {
    this._costHistory.append((t, v))
    this._cost = v
  }


  /** Checks whether another object equals this one
    *
    * @param other another object to test equality for
    * @return boolean indicating if the two objects are the same
    */
  override def equals(other: Any): Boolean =
    other match {
      case that: MyEdge => that.canEqual(this) && this.startVertex == that.startVertex && this.endVertex == that.endVertex
      case _ => false
    }

  /** Checks whether we are allowed to compare this object to another
    *
    * @param other
    * @return
    */
  def canEqual(other: Any): Boolean = other.isInstanceOf[MyEdge]


  /** Definition of equality.
    *
    * @return Int representing the object
    */
  override def hashCode: Int = {
    (this.startVertex, this.endVertex).##
  }

  override def toString: String = this.ID

  def toJSON: String = {
    "{\n" +
      "\"ID\":\"" + this.ID + "\",\n" +
      "\"start_vertex\":\"" + this.startVertex.name + "\",\n" +
      "\"end_vertex\":\"" + this.endVertex.name + "\",\n" +
    "\"cost_history\":[" + this._costHistory.map(tc => "{\"t\":" + tc._1 + ", \"c\":" + tc._2 + "}").mkString(",\n") + "]\n"+
      "}"
  }


  def deepCopy: MyEdge = new MyEdge(
    this.startVertex, this.endVertex
  )
}