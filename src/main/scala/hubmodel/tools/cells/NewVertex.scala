package hubmodel.tools.cells

import hubmodel.{Position, generateUUID}

trait NewVertex {

  val ID: String = generateUUID

  def center: Position
  def area: Double
  def corners: List[Position]
  def numberCorners: Int = corners.size
  def isInside(p: Position): Boolean

  def name: String
  // unique identifier
  def uniformSamplePointInside: Position
  //def equalsID(other: Any): Boolean
  override def equals(other: Any): Boolean
  override def hashCode: Int
  override def toString: String
  def nameCompare(n: String): Boolean = this.name == n
}
