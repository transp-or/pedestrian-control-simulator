package hubmodel.tools

import java.awt.Color

import hubmodel.Position

trait MyCellComputationTrait {
  def center: Position
  def area: Double
  def angles: List[Position]
  def numberAngles: Int = angles.size
  def isInside(p: Position): Boolean
}

trait MyCellRepresentationTrait{
  def scalarToShow: Double
  def scalarToColor: Color
  def stringToShow: String
  def horizontalMaxTextWidth: Double
  def xCoords: Array[Double]
  def yCoords:Array[Double]
}

trait MyCellTrait extends MyCellComputationTrait with MyCellRepresentationTrait