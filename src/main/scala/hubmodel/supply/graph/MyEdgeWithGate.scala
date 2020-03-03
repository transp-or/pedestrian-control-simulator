package hubmodel.supply.graph

import hubmodel.Position
import hubmodel.ped.PedestrianSim
import tools.Time
import tools.cells.Vertex

abstract class MyEdgeWithGate(override val startVertex: Vertex, override val endVertex: Vertex, val start: Position, val end: Position, val monitoredArea: String) extends MyEdge(startVertex, endVertex) {



  // Physical width of the gate. Required to compute the maximum flow.
  val width: Double = (end - start).norm

  // private container for the flow rate of the gate [pax/s]
  private var _flowRate: Double = this.width * 1.0

  // Container for storing the states of the flow gates
  val positionHistory: collection.mutable.ArrayBuffer[(Time, Double)] = collection.mutable.ArrayBuffer()


  // The list of pedestrians which are waiting at the gate to be let through
  val pedestrianQueue: scala.collection.mutable.Queue[PedestrianSim] = scala.collection.mutable.Queue[PedestrianSim]()


  /**
    * Getter method for the flow rate
    *
    * @return flow rate
    */
  def flowRate: Double = this._flowRate

  /**
    * Updates the flow rate and appends the value to the history of positions of the gates.
    *
    * @param newFlowRate new flow rate
    * @param currentTime start of the time interval for which this flow rates is valid
    */
  def setFlowRate(newFlowRate: Double, currentTime: Time): Unit = {
    this._flowRate = newFlowRate
    this.positionHistory.append((currentTime, newFlowRate))
  }
}