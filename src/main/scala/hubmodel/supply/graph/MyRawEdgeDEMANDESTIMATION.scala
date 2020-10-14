package hubmodel.supply.graph

import hubmodel.generateUUID
import org.jgrapht.graph.DefaultWeightedEdge
import tools.cells.Vertex

class MyRawEdgeDEMANDESTIMATION[T <: Vertex](val startVertex: T, val endVertex: T) extends DefaultWeightedEdge {


  // ID of the edge
  val ID: String = generateUUID

  // distance between vertices in straight line.
  val length: Double = (startVertex.center - endVertex.center).norm

  // Cost of the edge
  private var _cost: Double = length

  // accessor for the cost
  def cost: Double = _cost

  // setter for the cost.
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

  def deepCopy: MyRawEdgeDEMANDESTIMATION[T] = new MyRawEdgeDEMANDESTIMATION[T](
    this.startVertex, this.endVertex
  )

  def this(start: T, end: T, cost: Double) = {
    this(start, end)
    updateCost(cost)
  }

}
