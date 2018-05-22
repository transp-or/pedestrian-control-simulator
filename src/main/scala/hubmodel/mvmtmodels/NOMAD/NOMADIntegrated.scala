package hubmodel.mvmtmodels.NOMAD

import hubmodel.DES.{Action, SFGraphSimulator}
import hubmodel.{Position, Time}
import hubmodel.TimeNumeric.mkOrderingOps
import hubmodel.ped.PedestrianNOMAD
import hubmodel.supply.continuous.Wall
import nl.tudelft.pedestrians.agents.PedestrianType//.{getMaxExtentPed, getPedsMinRadius, getPedsMaxSpeed}


class NOMADIntegrated(sim: SFGraphSimulator) extends Action {

  /** Finds the closest end point of a wall if the point is not on the wall
    *
    * @param point point which is not on the wall
    * @param w     wall to finds end from
    * @return closest end point of the wall to the point
    */
  protected def getClosestEndPoint(point: Position, w: Wall): Position = {
    if ((w.startPoint - point).norm < (w.endPoint - point).norm) {
      w.startPoint
    }
    else {
      w.endPoint
    }
  }

  /** Find the point used to compute the repulsive effects from the wall onto a pedestrian.
    *
    * @param pos Position of the pedestrian
    * @param w   wall to calculate repulsion from
    * @return position used to calculate repulsion
    */
  protected def getClosestPoint(pos: Position, w: Wall): Position = {
    val wallDir: Position = (w.endPoint - w.startPoint).normalized

    val proj: Position = {
      val AP: Position = pos - w.startPoint
      wallDir * (wallDir dot AP)// + w.startPoint
    }
    if (proj.dot(wallDir) > 0.0 && proj.norm <= (w.endPoint-w.startPoint).norm) proj + w.startPoint//computeProjection(pos, w)
    //if (!isOnWall(proj, w)) getClosestEndPoint(pos, w)
    else getClosestEndPoint(pos, w)
  }


  protected def updatePedIsolation(currentTime: Time, p: PedestrianNOMAD): Unit = { // may should make a distinction and not update the isolation type for those pedestrians
    // that are in collision and maybe even those that are in-range at every step but just once in a while.
    var isolationInterval = 0
    // get the distance of the closest pedestrian in the simulation from this
    //val minDistance = this.level.getPositionManager.getClosestPedDistance(this)
    // calculate the isolated time
    //isolationInterval = Pedestrian.calculateIsolationIntervalPed(minDistance)
    // if it is negative then check for in-range
    val pedInsideIsoltionDistance: Iterable[PedestrianNOMAD] = sim.findNeighbours(p.ID, (PedestrianType.getMaxExtentPed - PedestrianType.getPedsMaxRadius) /(2 * PedestrianType.getPedsMaxSpeed))
    val NEEDTOIMPLEMENTFINDCLOSESTNEIGOUR = 10.0
    if ( pedInsideIsoltionDistance.nonEmpty) {
      val closestPed = pedInsideIsoltionDistance.minBy(that => (p.currentPosition - that.currentPosition).norm)
      if ((closestPed.currentPosition - p.currentPosition).norm - 2*PedestrianType.getPedsMaxRadius / (2*PedestrianType.getPedsMaxSpeed) <= sim.sf_dt.value) {
        p.isolationTimePed = currentTime.addDouble(PedestrianType.getMaxExtentPed - 2*PedestrianType.getPedsMaxRadius / (2*PedestrianType.getPedsMaxSpeed))
        p.isolationTypePed =  hubmodel.IN_COLLISION
      } else {
        p.isolationTimePed = currentTime.addDouble((closestPed.currentPosition - p.currentPosition).norm - 2*PedestrianType.getPedsMaxRadius / (2*PedestrianType.getPedsMaxSpeed))
        p.isolationTypePed =  hubmodel.IN_RANGE
      }
    } else {
      p.isolationTimePed = sim.currentTime.addDouble(NEEDTOIMPLEMENTFINDCLOSESTNEIGOUR - (PedestrianType.getMaxExtentPed - PedestrianType.getPedsMaxRadius) / (2 * PedestrianType.getPedsMaxSpeed))
      p.isolationTypePed = hubmodel.ISOLATED
    }
  }



  def updateObsIsolation(time: Time): Unit {

    // may should make a distinction and not update the isolation type for those pedestrians
    // that are in collision at every step but just once in a while.
    // initialise  the min distance between two pedestrians
    //int isolationInterval;

    double minDistance = this.level.getPositionManager().getClosestObsDistance(this);
    //double nextDistance;
    int isolationInterval;

    // calculate the isolated time
    isolationInterval = Pedestrian.calculateIsolationTimeObs(minDistance);
    // if it is negative then check for in-range
    if (isolationInterval <= NomadModel.model.simTime.getTimeStep()){
      // calculate the in-range time
      isolationInterval = Pedestrian.calculateInRangeTimeObs(minDistance);

      if (isolationInterval <= NomadModel.model.simTime.getTimeStep()){
        // if the in-range time step is smaller then the time step
        // then the pedestrian should be in collision.
        // Add an extra value to the calculate in collision interval
        // to prevent an update at the next X steps.
        this.isolationTimeObs = currentTime + collisionIntervalObs;
        //return the in-collision type
        this.isolationTypeObs = NOMAD.IN_COLLISION;
      }else {
        // if the in range time step is larger then the time step
        // then add the current time to calculate the next time that
        // this pedestrian should update his isolation state.
        this.isolationTimeObs = currentTime + isolationInterval;
        //return the in-range type
        this.isolationTypeObs =  NOMAD.IN_RANGE;
      }

    }else {
      // if the isolated time step is positive then add the current time
      this.isolationTimeObs = currentTime + isolationInterval;
      //return the isolated type
      this.isolationTypeObs =  NOMAD.ISOLATED;
    }

  }


  def updateIsolation(time: Time, p: PedestrianNOMAD): Unit = {
    if (p.isVariableStep) {
      if (p.isolationTimePed < time) { updatePedIsolation(time, p) }

      if (p.isolationTimeObs < time) { updateObsIsolation(time) }
    }
  }

  override def execute(): Unit = {

    sim.population.foreach(ped => {

      // The pedestrian step function deals with the state of the ped (entering, walking, activity) and is not reauired here.
      // Only the "stepWalkingData" functionalites are implemented
      //ped.step(sim.currentTime) WRITTEN INLINE
      // in case he is not invisible for the simulation
      if (!ped.isInvisible) {
        /* update the pedestrian data */
        //this.level.updateCurrentCellAndWalkable(this) UNUSED IN THIS FRAMEWORK

        // check if it is time for the pedestrian to check his isolation times
        this.updateIsolation(sim.currentTime, ped)


        // the distance travelled could be calculated directly in the move method
        // of the walkable horizontal preventing the need of the previous position field.
        // However it would always require a care when implementing new
        // walking behaviours like the event movement to prevent this field to be forgotten.
        // putting it here and using the new field previousPosition makes it unambiguous.
        // NOTE: for the pedestrians moving according to the event step it is not totally accurate because of the
        // intermediate steps, the pedestrians in general have walked more then this difference but we keep it to
        // be consistent with the time interval of the simulation and neglecting the in-between movements.
        // IN THIS CASE, ALWAYS UPDATE THE TRAVELLED DISTANCE
        //if (this.isUpdateWalkingData)
        ped.travelDistance += (ped.currentPosition - ped.getHistoryPosition.last._2).norm
        ped.updatePreviousPositionAndSpeed(sim.currentTime)
      }

      if (!sim.finalDestinationReached(ped)) {
        this.insertPedInMoveList(ped)
      } else {
        this.pedestriansToExit.add(ped)
      }

    })
    this.movePedestrians(sim.currentTime)
    this.pedestriansToExit.foreach(p => this.exitPedestrianFromSimulation(p, sim.currentTime))
  }

}
