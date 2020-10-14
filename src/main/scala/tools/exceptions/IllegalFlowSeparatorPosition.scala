package tools.exceptions

class IllegalFlowSeparatorPosition(message: String, val position: Double) extends IllegalPhysicalQuantity(message)
class IllegalMovingWalkwaySpeed(message: String, val position: Double) extends IllegalPhysicalQuantity(message)
