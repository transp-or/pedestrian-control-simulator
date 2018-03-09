package hubmodel.tools

import java.awt.Color

import hubmodel.NewBetterPosition2D

trait MyCellComputationTrait {
  def center: NewBetterPosition2D
  def area: Double
  def angles: List[NewBetterPosition2D]
  def numberAngles: Int = angles.size
  def isInside(p: NewBetterPosition2D): Boolean
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