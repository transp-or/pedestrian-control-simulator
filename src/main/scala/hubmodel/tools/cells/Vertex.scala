package hubmodel.tools.cells

import hubmodel.{Position, Time, VertexID, generateUUID}

trait Vertex {

  val ID: VertexID = generateUUID

  def center: Position

  def area: Double

  def corners: Vector[Position]

  def numberCorners: Int = corners.size

  def isInside(p: Position): Boolean

  def name: String

  def isOD: Boolean

  def generationRate: Double

  // unique identifier
  def uniformSamplePointInside: Position

  //def equalsID(other: Any): Boolean
  override def equals(other: Any): Boolean

  override def hashCode: Int

  override def toString: String

  def nameCompare(n: String): Boolean = this.name == n
}
