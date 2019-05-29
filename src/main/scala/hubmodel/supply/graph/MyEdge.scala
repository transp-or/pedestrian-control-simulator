package hubmodel.supply.graph

import hubmodel.generateUUID
import hubmodel.tools.cells.Rectangle
import org.jgrapht.graph.DefaultWeightedEdge

/** Representation of an edge used in the graph structure. Can be used as parent for advanced edges with gates.
  *
  * @param startVertex vertex at origin
  * @param endVertex   vertex at destination
  */
class MyEdge(val startVertex: Rectangle, val endVertex: Rectangle) extends DefaultWeightedEdge {

  // ID of the edge
  val ID: String = generateUUID

  // distance between vertices in straight line.
  val length: Double = (startVertex.center - endVertex.center).norm

  // Cost of the edge
  private var _cost: Double = length

  // accessor for the cost
  def cost: Double = _cost

  // setter for the cost. The call to sychronized is to ensure multi-thread correctness
  def updateCost(v: Double): Unit = _cost = v


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

  def deepCopy: MyEdge = new MyEdge(
    this.startVertex, this.endVertex
  )
}