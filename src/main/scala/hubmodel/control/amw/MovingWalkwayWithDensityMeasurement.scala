package hubmodel.control.amw

import hubmodel.DES.NOMADGraphSimulator
import hubmodel.Position
import hubmodel.control.flowsep.FlowLine
import hubmodel.control.{BidirectionalFlow, ControlDevicePolicy, Density, FunctionalForm, FunctionalFormMovingWalkwayDensity, Measurement, MovingWalkwaySpeed}
import hubmodel.supply.graph.{GraphContainer, MyEdge, RouteGraph}
import myscala.math.stats.Quantile
import optimization.ALNS.Policy
import tools.Time
import tools.TimeNumeric.mkOrderingOps
import tools.cells.{DensityMeasuredArea, Vertex}

class MovingWalkwayWithDensityMeasurement[T <: Density, U <: MovingWalkwaySpeed](name:String,
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
                                                                                 val criticalAreaStart: Iterable[DensityMeasuredArea],
                                                                                 val criticalAreaEnd: Iterable[DensityMeasuredArea],
                                                                                 val PIGains: (Double, Double)) extends MovingWalkwayAbstract(name, firstVertex, secondVertex, width, start, end, associatedZonesStart, associatedZonesEnd, droppedVertices, associatedConnectivity, parallelFlows) {

  // History of the measured flows.
  private val flowHistoryNew: collection.mutable.ArrayBuffer[(Time, BidirectionalFlow)] = collection.mutable.ArrayBuffer()

  private def computeQuantile(data: Vector[Double]): Double = {
    if(data.isEmpty) {0.0} else {
      myscala.math.stats.computeQuantile(75.0)(data).value
    }
  }

  private var currentMovingDirection: Int = 1

  private var nextPossibleDirectionChange: Time = Time(0)

  def updateFlowHistory(t: Time): Unit = {
    this.flowHistoryNew.append((t, BidirectionalFlow(this.inflowLinesStart.map(l => l.fractionKept * l.fl.getPedestrianFlow).sum, this.inflowLinesEnd.map(l => l.fractionKept * l.fl.getPedestrianFlow).sum)))
  }

  private def computeDirection(t: Time): Unit = {
    //flowHistoryNew.append((t, BidirectionalFlow(this.inflowLinesStart.map(l => l.fractionKept * l.getPedestrianFlow).sum, this.inflowLinesEnd.map(l => l.fractionKept * l.getPedestrianFlow).sum)))

    /*if (this.criticalAreadStart.forall(a => a.paxIndividualDensityHistory.last._2.isEmpty) || this.criticalAreadEnd.forall(a => a.paxIndividualDensityHistory.last._2.isEmpty)) {0.0}
    else if (computeQuantile(this.criticalAreadStart.flatMap(_.paxIndividualDensityHistory.last._2).toVector) < computeQuantile(this.criticalAreadEnd.flatMap(_.paxIndividualDensityHistory.last._2).toVector)) {1.0}
    else {-1.0}*/


    if (t >= nextPossibleDirectionChange && currentMovingDirection == 1 && flowHistoryNew.last._2.f2 > 1.5 * flowHistoryNew.last._2.f1) {
      nextPossibleDirectionChange = t + Time(30)
      currentMovingDirection = -1
    } else if (t >= nextPossibleDirectionChange && currentMovingDirection == -1 && flowHistoryNew.last._2.f1 > 1.5 * flowHistoryNew.last._2.f2) {
      nextPossibleDirectionChange = t + Time(30)
      currentMovingDirection = 1
    }
  }


  private def computeTargetSpeed(t: Time, finalTime:Time, direction: Double): AMWPolicy = {
    val nextSpeed: Double = if (direction == 1) {
      if (this.criticalAreaEnd.forall(a => a.paxIndividualDensityHistory.size < 2)) {
        3.0
      } else {
        //val ek: Double = (this.criticalAreaEnd.head.targetDensity - (1.0/3.0) * (computeQuantile(this.criticalAreaEnd.flatMap(_.paxIndividualDensityHistory.last._2).toVector) + computeQuantile(this.criticalAreaEnd.flatMap(_.paxIndividualDensityHistory.dropRight(1).last._2).toVector) + computeQuantile(this.criticalAreaEnd.flatMap(_.paxIndividualDensityHistory.dropRight(2).last._2).toVector)))
        val ek: Double = this.criticalAreaEnd.head.targetDensity - computeQuantile(this.criticalAreaEnd.flatMap(_.paxIndividualDensityHistory.last._2).toVector)
        //val ekm1: Double = (this.criticalAreaEnd.head.targetDensity - (1.0/3.0) * (computeQuantile(this.criticalAreaEnd.flatMap(_.paxIndividualDensityHistory.dropRight(1).last._2).toVector) + computeQuantile(this.criticalAreaEnd.flatMap(_.paxIndividualDensityHistory.dropRight(2).last._2).toVector) + computeQuantile(this.criticalAreaEnd.flatMap(_.paxIndividualDensityHistory.dropRight(3).last._2).toVector)))
        val ekm1: Double = this.criticalAreaEnd.head.targetDensity - computeQuantile(this.criticalAreaEnd.flatMap(_.paxIndividualDensityHistory.dropRight(1).last._2).toVector)
        val s = this._nextSpeed + (PIGains._1 + PIGains._2) * ek  - PIGains._1 * ekm1
        //println(t, direction, s, ek, ekm1)
        math.round(1.0*math.max(0.0,math.min(s, 3.0)))/1.0
      }
    } else if (direction == -1) {
      if (this.criticalAreaStart.forall(a => a.paxIndividualDensityHistory.size < 2)) {
        -3.0
      } else {
        //val ek: Double = (this.criticalAreaStart.head.targetDensity - (1.0/3.0) * (computeQuantile(this.criticalAreaStart.flatMap(_.paxIndividualDensityHistory.last._2).toVector) + computeQuantile(this.criticalAreaStart.flatMap(_.paxIndividualDensityHistory.dropRight(1).last._2).toVector) + computeQuantile(this.criticalAreaStart.flatMap(_.paxIndividualDensityHistory.dropRight(2).last._2).toVector)))
        val ek: Double = this.criticalAreaStart.head.targetDensity - computeQuantile(this.criticalAreaStart.flatMap(_.paxIndividualDensityHistory.last._2).toVector)
        //val ekm1: Double = (this.criticalAreaStart.head.targetDensity - (1.0/3.0) * (computeQuantile(this.criticalAreaStart.flatMap(_.paxIndividualDensityHistory.dropRight(1).last._2).toVector) + computeQuantile(this.criticalAreaStart.flatMap(_.paxIndividualDensityHistory.dropRight(2).last._2).toVector) + computeQuantile(this.criticalAreaStart.flatMap(_.paxIndividualDensityHistory.dropRight(3).last._2).toVector)))
        val ekm1: Double = this.criticalAreaStart.head.targetDensity - computeQuantile(this.criticalAreaStart.flatMap(_.paxIndividualDensityHistory.dropRight(1).last._2).toVector)
        val s = math.min(0.0, this._nextSpeed) - ((PIGains._1 + PIGains._2) * ek  - PIGains._1 * ekm1)
        //println(t, direction, s, ek, ekm1)
        math.round(1.0*math.min(0.0,math.max(s, -3.0)))/1.0
      }
    } else {
      0.0
    }
    /*val speed: MovingWalkwaySpeed = PIGains match {
      case pos if direction == 1.0 => {
        this.criticalAreadEnd.head.targetDensity
        MovingWalkwaySpeed(direction * f.f(Density(computeQuantile(this.criticalAreadEnd.flatMap(_.paxIndividualDensityHistory.last._2).toVector).value)).s)
      }
      case neg if direction == -1.0 => MovingWalkwaySpeed(direction * f.f(Density(computeQuantile(this.criticalAreadStart.flatMap(_.paxIndividualDensityHistory.last._2).toVector).value)).s)
      case _ => MovingWalkwaySpeed(3.0)
    }*/

    AMWPolicy(this.name, t, finalTime, nextSpeed, this.length)
  }

  /** Makes the new control policy feasible with the previous existing speed
    *
    * @param t
    * @param p
    * @return
    */
  private def computeControlPolicy(t: Time, p: AMWPolicy): (Policy, Vector[MovingWalkwayControlEvents]) = {
    optimization.ALNS.enforceSpeedChangeIntoPolicy(Vector(p), Map(this.name -> this.speed(t)))
  }

  def updateReactivePolicy(t: Time, sim: NOMADGraphSimulator): Unit = {

    if (!this.isClosed && this.noControlPolicy) {
      val newPolicy = computeControlPolicy(t, computeTargetSpeed(t, sim.finalTime, this.currentMovingDirection))


      this.setControlPolicy(
        newPolicy._1.x.collect { case w: AMWPolicy if w.name == this.name => {w}
        }, newPolicy._2.find(_.name == this.name)
      )
      this.insertChangeSpeed(sim)
    }
  }


  def updateDirection(t: Time): Unit = {
    this.computeDirection(t)
  }

  def deepCopyPIGains(graph: GraphContainer, flowLines: Iterable[FlowLine], areas: Iterable[DensityMeasuredArea],  P: Double, I: Double): MovingWalkwayWithDensityMeasurement[T, U] = {
    new MovingWalkwayWithDensityMeasurement[T, U](
      this.name,
      graph.vertexMapNew(this.firstVertex.name),
      graph.vertexMapNew(this.secondVertex.name),
      this.width,
      this.start,
      this.end,
      associatedZonesStart.map(v => graph.vertexMapNew(v.name)),
      associatedZonesEnd.map(v => graph.vertexMapNew(v.name)),
      this.droppedVertices,
      this.associatedConnectivity.map(e => graph.edges.find(ge => ge.startVertex == e.startVertex && ge.endVertex == e.endVertex).get),
      this.parallelFlows.map(pf => pf.map(v => graph.vertexMapNew(v.name))),
      this.inflowLinesStart.map(fl => new FlowLineWithFraction(fl.name, flowLines.find(_.name == fl.name).get, fl.fractionKept)),
      this.inflowLinesEnd.map(fl => new FlowLineWithFraction(fl.name, flowLines.find(_.name == fl.name).get, fl.fractionKept)),
      this.criticalAreaStart.map(a => areas.find(_.name == a.name).get),
      this.criticalAreaEnd.map(a => areas.find(_.name == a.name).get),
      (P, I)
    )
  }


}

