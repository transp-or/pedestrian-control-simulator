package hubmodel.control.amw

import hubmodel.DES.NOMADGraphSimulator
import hubmodel.Position
import hubmodel.control.{BidirectionalFlow, ControlDevicePolicy, FunctionalForm, FunctionalFormMovingWalkway, Measurement, MovingWalkwaySpeed, SeparatorPositionFraction}
import hubmodel.control.flowsep.FlowLine
import hubmodel.supply.graph.MyEdge
import tools.Time
import tools.cells.Vertex

/** Takes flow measurements from the inflow lines specified at each end and sets the direction to match the largest
  * expected flow.
  *
  * @param name
  * @param firstVertex
  * @param secondVertex
  * @param width
  * @param start
  * @param end
  * @param associatedZonesStart
  * @param associatedZonesEnd
  * @param droppedVertices
  * @param associatedConnectivity
  * @param parallelFlows
  * @param startArea
  * @param endArea
  * @param inflowLinesStart
  * @param inflowLinesEnd
  * @param function
  * @tparam T
  * @tparam U
  */
class MovingWalkwayWithFlowMeasurement[T <: Measurement, U <: MovingWalkwaySpeed](name: String,
                                                                                  firstVertex: Vertex,
                                                                                  secondVertex: Vertex,
                                                                                  width: Double,
                                                                                  start: Position,
                                                                                  end: Position,
                                                                                  associatedZonesStart: Vector[Vertex],
                                                                                  associatedZonesEnd: Vector[Vertex],
                                                                                  droppedVertices: Vector[String],
                                                                                  associatedConnectivity: Iterable[MyEdge],
                                                                                  parallelFlows: Vector[Vector[Vertex]],
                                                                                  val inflowLinesStart: Iterable[FlowLineWithFraction],
                                                                                  val inflowLinesEnd: Iterable[FlowLineWithFraction],
                                                                                  val function: FunctionalForm[T, U]) extends MovingWalkway(name, firstVertex, secondVertex, width, start, end, associatedZonesStart, associatedZonesEnd, droppedVertices, associatedConnectivity, parallelFlows) {

  // History of the measured flows.
  private val flowHistoryNew: collection.mutable.ArrayBuffer[(Time, BidirectionalFlow)] = collection.mutable.ArrayBuffer()

  /** Computes the new speed which should be implented in the AMW
    *
    * @param t
    */
  private def computeTargetSpeed(t: Time, finalTime:Time): AMWPolicy = {
    flowHistoryNew.append((t, BidirectionalFlow(this.inflowLinesStart.map(l => l.fractionKept * l.getPedestrianFlow).sum, this.inflowLinesEnd.map(l => l.fractionKept * l.getPedestrianFlow).sum)))

    val speed: MovingWalkwaySpeed = function match {
      case default: FunctionalFormMovingWalkway => {
        default.f(flowHistoryNew.last._2)
      }
      case _ => {throw new NotImplementedError("This does not make sense for AMWs")}
    }

    AMWPolicy(this.name, t, finalTime, speed.s, this.length)
  }

  /** Makes the new control policy feasible with the previous existing speed
    *
    * @param t
    * @param p
    * @return
    */
  private def computeControlPolicy(t: Time, p: AMWPolicy): (Vector[ControlDevicePolicy], Vector[MovingWalkwayControlEvents]) = {
    optimization.ALNS.enforceSpeedChangeIntoPolicy(Vector(p), Map(this.name -> this.speed(t)))
  }

  def updateReactivePolicy(t: Time, sim: NOMADGraphSimulator): Unit = {

    val newPolicy = computeControlPolicy(t, computeTargetSpeed(t, sim.finalTime))
    this.setControlPolicy(
      newPolicy._1.collect{case w: AMWPolicy if w.name == this.name => {w}}, newPolicy._2.find(_.name == this.name)
    )
    this.insertChangeSpeed(sim)
  }

}
