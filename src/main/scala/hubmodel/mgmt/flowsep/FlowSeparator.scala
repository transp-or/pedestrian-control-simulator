package hubmodel.mgmt.flowsep

import hubmodel.DES.{Action, NOMADGraphSimulator}
import hubmodel.mgmt.flowgate._
import hubmodel.ped.PedestrianNOMAD
import hubmodel.supply.continuous.{MovableWall, SINGLELINE, Wall}
import hubmodel.supply.graph.MyEdge
import hubmodel.tools.Time
import hubmodel.tools.cells.RectangleModifiable
import hubmodel.{FLOW_SEPARATOR_UPDATE, Position, generateUUID}

import scala.util.{Failure, Success, Try}

class FlowSeparator[T <: Measurement, U <: SeparatorPositionFraction](val startA: Position,
                                                   val startB: Position,
                                                   val endA: Position,
                                                   val endB: Position,
                                                   val inflowLinesStart: Iterable[FlowLine],
                                                   val inflowLinesEnd: Iterable[FlowLine],
                                                   val associatedZonesStart: Iterable[RectangleModifiable],
                                                   val associatedZonesEnd: Iterable[RectangleModifiable],
                                                   val associatedConnectivity: Iterable[MyEdge],
                                                   val overridenZones: Iterable[String],
                                                   val function: FunctionalForm[T, U]) {

  val ID: String = generateUUID

  var start: Position = startA + (startB - startA) * 0.5
  var end: Position = endA + (endB - endA) * 0.5

  private var currentTargetPosition: (Position, Position) = (start, end)

  private val positionHistory: collection.mutable.ArrayBuffer[(Time, Position, Position)] = collection.mutable.ArrayBuffer()

  @deprecated
  private val flowHistory: collection.mutable.ArrayBuffer[(Time, Int, Int)] = collection.mutable.ArrayBuffer()

  private val flowHistoryNew: collection.mutable.ArrayBuffer[(Time, BidirectionalFlow)] = collection.mutable.ArrayBuffer()

  private val fractionHistory: collection.mutable.ArrayBuffer[(Time, Double)] = collection.mutable.ArrayBuffer((Time(0.0), 0.5))

  private val SPEED: Double = 0.25 // m/s fixed in a arbitrary manner

  var movingWallEventIsInserted: Boolean = false

  def flowSeparatorNeedsToMove(updateStep: Time): Boolean = {
    (this.start - this.currentTargetPosition._1).norm > this.SPEED * updateStep.value && (this.end - this.currentTargetPosition._2).norm > this.SPEED * updateStep.value
  }

  def getPositionHistory: IndexedSeq[(Time, Position, Position)] = positionHistory.toVector

  //def getFlowHistory: IndexedSeq[(Time, Int, Int)] = flowHistory.toVector

  def getFractionHistory: IndexedSeq[(Time, Double)] = fractionHistory.toVector


  def initializePositionHistory(startTime: Time): Unit = {
    positionHistory.append((startTime, start, end))
  }

  def updateWallTargetPosition(time: Time): Unit = {

    flowHistoryNew.append((time, BidirectionalFlow(this.inflowLinesStart.map(_.getPedestrianFlow).sum, this.inflowLinesEnd.map(_.getPedestrianFlow).sum)))

    function match {
      case default: FunctionalFormFlowSeparator => {
        val frac: SeparatorPositionFraction = Try(default.f(flowHistoryNew.last._2)) match {
          case Success(s) => s
          case Failure(f) => f match {
            case illegalArg: IllegalArgumentException => {
              println("Warning ! Error when computing flow separator fraction, defaulting to 0.5")
              SeparatorPositionFraction(0.5)
            }
            case _ => throw f
          }
        }


        if (math.abs(frac.r - this.fractionHistory.last._2) / this.fractionHistory.last._2 > FLOW_SEPARATOR_UPDATE) {
          this.currentTargetPosition = (this.startA + (this.startB - this.startA) * frac.r, this.endA + (this.endB - this.endA) * frac.r)
          this.associatedZonesStart.foreach(_.setTargetPosition(frac.r))
          this.associatedZonesEnd.foreach(_.setTargetPosition(frac.r))
        }
        this.fractionHistory.append((time, frac.r))

      }
      case _ => throw new NotImplementedError("This does not make sense for flow separators")
    }
  }

  def getWall: Wall = {
    new MovableWall("movable wall", this.start, this.end, SINGLELINE)
  }

  class MoveFlowSeperator[V <: PedestrianNOMAD](sim: NOMADGraphSimulator[V]) extends Action {


    override def execute(): Unit = {
      if ((currentTargetPosition._1 - start).norm > 0.0) {
        start = start + (currentTargetPosition._1 - start).normalized * (SPEED * sim.sf_dt.value.toDouble)
        associatedZonesStart.foreach(_.moveRectangle(sim.sf_dt))
      }

      if ((currentTargetPosition._2 - end).norm > 0.0) {
        end = end + (currentTargetPosition._2 - end).normalized * (SPEED * sim.sf_dt.value.toDouble)
        associatedZonesEnd.foreach(_.moveRectangle(sim.sf_dt))
      }

      positionHistory.append((sim.currentTime, start, end))

      if (flowSeparatorNeedsToMove(sim.sf_dt)) {
        sim.insertEventWithDelay(sim.sf_dt)(new MoveFlowSeperator(sim))
        movingWallEventIsInserted = true
      } else {
        //start = currentTargetPosition._1
        //end = currentTargetPosition._2
        movingWallEventIsInserted = false
      }
    }
  }

  override def clone(): FlowSeparator[T, U] = new FlowSeparator[T, U](
    this.startA,
    this.startB,
    this.endA,
    this.endB,
    this.inflowLinesStart.map(_.clone()),
    this.inflowLinesEnd.map(_.clone()),
    this.associatedZonesStart.map(_.clone()),
    this.associatedZonesEnd.map(_.clone()),
    this.associatedConnectivity.map(_.clone()),
    this.overridenZones,
    this.function
  )

  def cloneChangeFunction[V <: Measurement, W <: SeparatorPositionFraction](newFunction: FunctionalForm[V, W]): FlowSeparator[V, W] = new FlowSeparator[V, W](
    this.startA,
    this.startB,
    this.endA,
    this.endB,
    this.inflowLinesStart.map(_.clone()),
    this.inflowLinesEnd.map(_.clone()),
    this.associatedZonesStart.map(_.clone()),
    this.associatedZonesEnd.map(_.clone()),
    this.associatedConnectivity.map(_.clone()),
    this.overridenZones,
    newFunction
  )
}
