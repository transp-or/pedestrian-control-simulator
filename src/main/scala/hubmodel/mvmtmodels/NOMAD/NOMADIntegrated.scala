package hubmodel.mvmtmodels.NOMAD

import java.util
import java.util.concurrent.ThreadLocalRandom

import com.vividsolutions.jts.geom.Coordinate
import hubmodel.DES.{Action, NOMADGraphSimulator}
import hubmodel.ped.PedestrianNOMAD
import hubmodel.supply.continuous.Wall
import hubmodel.tools.cells.isInVertex
import hubmodel.{Position, Time}
import javax.vecmath.Vector3d
import myscala.math.vector.Vector2D
import nl.tudelft.pedestrians.agents.WalkingBehavior.{pedestrianPhysical, pedestrianRepellOpposing}
import nl.tudelft.pedestrians.collection.InfluenceAreaReturnPedData
import nl.tudelft.pedestrians.utils.GeometryUtils
import nomad.operational.InfluenceAreaReturnObsData

class NOMADIntegrated[T <: PedestrianNOMAD](sim: NOMADGraphSimulator[T]) extends Action {

  // initialise the in range and in collision time schedule
  //val isolatedTimeStepMillis: Double = 1000.0 * sim.sf_dt.value // NomadModel.model.simTime.getTimeStep
  val isolatedTimeStepSeconds: Double = sim.sf_dt.value.toDouble //NomadModel.model.simTime.getTimeStepSeconds

  //val rangeTimeStepMillis: Double = this.isolatedTimeStepMillis * 0.2 //NOMAD.defaults.IN_RANGE_FRACTION.round
  val rangeTimeStepSeconds: Double = isolatedTimeStepSeconds * 0.2 //isolatedTimeStepSeconds * 0.2 // SimulationTime.convertSimTime(this.rangeTimeStepMillis)

  //this.inRangeSteps = (int) Math.floor(this.isolatedTimeStepMillis/this.rangeTimeStepMillis);
  //val remainderInRangeMillis: Double = this.isolatedTimeStepMillis % this.rangeTimeStepMillis
  val remainderInRangeSeconds: Double = 0.0 //this.isolatedTimeStepSeconds % this.rangeTimeStepSeconds // SimulationTime.convertSimTime(this.remainderInRangeMillis)

  //val collisionTimeStepMillis: Double = this.isolatedTimeStepMillis * 0.1 // NOMAD.defaults.IN_COLLISION_FRACTION.round
  val collisionTimeStepSeconds: Double = this.isolatedTimeStepSeconds * 0.1 // SimulationTime.convertSimTime(this.collisionTimeStepMillis)
  //val remainderInCollisionMillis: Double = this.isolatedTimeStepMillis % this.collisionTimeStepMillis
  val remainderInCollisionSeconds: Double = 0.0 //this.isolatedTimeStepSeconds % this.collisionTimeStepSeconds // SimulationTime.convertSimTime(this.remainderInCollisionMillis)


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
      wallDir * (wallDir dot AP) // + w.startPoint
    }
    if (proj.dot(wallDir) > 0.0 && proj.norm <= (w.endPoint - w.startPoint).norm) proj + w.startPoint //computeProjection(pos, w)
    //if (!isOnWall(proj, w)) getClosestEndPoint(pos, w)
    else getClosestEndPoint(pos, w)
  }


  protected def updatePedIsolation(currentTime: Time, p: PedestrianNOMAD): Unit = { // may should make a distinction and not update the isolation type for those pedestrians
    // that are in collision and maybe even those that are in-range at every step but just once in a while.
    // get the distance of the closest pedestrian in the simulation from this
    //val minDistance = this.level.getPositionManager.getClosestPedDistance(this)
    // calculate the isolated time
    //isolationInterval = Pedestrian.calculateIsolationIntervalPed(minDistance)
    // if it is negative then check for in-range
    if (sim.population.size > 1) {
      val closestPed: PedestrianNOMAD = sim.population.filter(ped => ped.ID != p.ID).minBy(other => (p.currentPosition - other.currentPosition).norm)
      val closestDistance: Double = (p.currentPosition - closestPed.currentPosition).norm
      var isolationInterval = (closestDistance - 3.0 - 0.3) / (2 * 3.2)

      val pedInsideIsoltionDistance: Iterable[PedestrianNOMAD] = sim.findNeighbours(p.ID, (3.0 - 0.3) / (2 * 3.2))
      val NEEDTOIMPLEMENTFINDCLOSESTNEIGOUR = closestDistance

      // PedestrianType.getPedsMaxRadius  = 0.3
      // PedestrianType.getMaxExtentPed = 3.0 or 0.75
      // PedestrianType.getPedsMaxSpeed =~ 2(1.34+0.25) = 3.2

      if (isolationInterval <= sim.sf_dt.value) {

        isolationInterval = (closestDistance - 2.0 * 0.3) / (2 * 3.2)
        //val closestPed = pedInsideIsoltionDistance.minBy(that => (p.currentPosition - that.currentPosition).norm)
        if (isolationInterval <= sim.sf_dt.value) {
          p.isolationTimePed = currentTime.addDouble(3.0 - 2 * 0.3 / (2 * 3.2)).value.toDouble
          p.isolationTypePed = hubmodel.IN_COLLISION
        } else {
          p.isolationTimePed = currentTime.addDouble((closestPed.currentPosition - p.currentPosition).norm - 2 * 0.3 / (2 * 3.2)).value.toDouble
          p.isolationTypePed = hubmodel.IN_RANGE
        }
      } else {
        p.isolationTimePed = sim.currentTime.addDouble(closestDistance - (3.0 - 0.3) / (2 * 3.2)).value.toDouble
        p.isolationTypePed = hubmodel.ISOLATED
      }
    }
  }


  def updateObsIsolation(time: Time, p: PedestrianNOMAD): Unit = {

    // may should make a distinction and not update the isolation type for those pedestrians
    // that are in collision at every step but just once in a while.
    // initialise  the min distance between two pedestrians
    //int isolationInterval;

    val minDistance: Double = sim.walls.map(w => (p.currentPosition - getClosestPoint(p.currentPosition, w)).norm).min

    //double nextDistance;
    // calculate the isolated time
    var isolationInterval: Double = (minDistance - 3.0) / 3.2

    // if it is negative then check for in-range
    if (isolationInterval <= sim.sf_dt.value) {
      // calculate the in-range time
      isolationInterval = (minDistance - 0.3) / 3.2

      if (isolationInterval <= sim.sf_dt.value) {
        // if the in-range time step is smaller then the time step
        // then the pedestrian should be in collision.
        // Add an extra value to the calculate in collision interval
        // to prevent an update at the next X steps.
        p.isolationTimeObs = sim.currentTime.addDouble((3.0 - 0.3) / 3.2).value.toDouble
        //return the in-collision type
        p.isolationTypeObs = hubmodel.IN_COLLISION

      } else {
        // if the in range time step is larger then the time step
        // then add the current time to calculate the next time that
        // this pedestrian should update his isolation state.
        p.isolationTimeObs = sim.currentTime.addDouble(isolationInterval).value.toDouble
        //return the in-range type
        p.isolationTypeObs = hubmodel.IN_RANGE
      }

    } else {
      // if the isolated time step is positive then add the current time
      p.isolationTimeObs = sim.currentTime.addDouble(isolationInterval).value.toDouble
      //return the isolated type
      p.isolationTypeObs = hubmodel.ISOLATED
    }


  }


  def updateIsolation(time: Time, p: PedestrianNOMAD): Unit = {
    if (p.isVariableStep) {
      //if (p.isolationTimePed < time.value) {
        updatePedIsolation(time, p)
      //}

      if (p.isolationTimeObs < time.value) {
        updateObsIsolation(time, p)
      }
    }
  }


  /** List with pedestrians that are in isolation in the current simulation step */
  private val pedestrianToMoveInIsolation = new collection.mutable.ArrayBuffer[PedestrianNOMAD]()
  /** List with pedestrians that are in range in the current simulation step */
  private val pedestrianToMoveInRange = new collection.mutable.ArrayBuffer[PedestrianNOMAD]()
  /** List with pedestrians that are in collision in the current simulation step */
  private val pedestrianToMoveInCollision = new collection.mutable.ArrayBuffer[PedestrianNOMAD]()

  //private val pedestriansToExit = new collection.mutable.ArrayBuffer[PedestrianNOMAD]()
  /**
    * Check if this pedestrian has to be included in the event step array.
    *
    * @param pedestrian
    */
  private def insertPedInMoveList(ped: PedestrianNOMAD): Unit = {

    //if (ped.isVariableStep)
    /*if (ped.isolationTypePed == hubmodel.IN_COLLISION || ped.isolationTypeObs == hubmodel.IN_COLLISION)*/ this.pedestrianToMoveInCollision.append(ped)
    /*else if (ped.isolationTypePed == hubmodel.IN_RANGE || ped.isolationTypeObs == hubmodel.IN_RANGE) this.pedestrianToMoveInRange.append(ped)
    else this.pedestrianToMoveInIsolation.append(ped)*/
    /*else { // if the time step is constant
      this.pedestrianToMoveInCollision.append(ped)
    }*/
    //this.pedestrianToMoveInCollision.append(ped)

  }


  override def execute(): Unit = {

    //sim.errorLogger.error("move peds @ " + sim.currentTime + ", nbr peds=" + sim.population.size)

    if (sim.useFlowSep) {
      sim.controlDevices.flowSeparators.foreach(fs => {
        fs.inflowLinesStart.foreach(fl => {
          fl.collectPedestriansWhoCrossed(sim.population)
        })
        fs.inflowLinesEnd.foreach(fl => {
          fl.collectPedestriansWhoCrossed(sim.population)
        })
      })
    }

    sim.population.foreach(ped => {ped.addHistory(sim.currentTime)})
    sim.population.filterNot(_.isWaiting).foreach(ped => {

      // The pedestrian step function deals with the state of the ped (entering, walking, activity) and is not reauired here.
      // Only the "stepWalkingData" functionalites are implemented
      //ped.step(sim.currentTime) WRITTEN INLINE
      // in case he is not invisible for the simulation
      if (!ped.isInvisible) {
        /* update the pedestrian data */
        //this.level.updateCurrentCellAndWalkable(this) UNUSED IN THIS FRAMEWORK

        // check if it is time for the pedestrian to check his isolation times

        ped.updateDesiredSpeed()
        this.updateIsolation(sim.currentTime, ped)
        ped.travelTime = sim.currentTime - ped.entryTime

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
        ped.previousMajorPosition = ped.currentPosition

        //ped.updatePreviousPositionAndSpeed(sim.currentTime)
        insertPedInMoveList(ped)
      }
    })

    /*if (sim.population.filterNot(_.isWaiting).size != (this.pedestrianToMoveInIsolation.size + this.pedestrianToMoveInRange.size + this.pedestrianToMoveInCollision.size)) {
      throw new Exception("error in size of population lists")
    }*/


    if (this.pedestrianToMoveInCollision.nonEmpty) {
      this.moveInCollisionStep()
    } else if (this.pedestrianToMoveInRange.nonEmpty) { // if at least one is in range and the rest is at isolation or in range
        // then the smallest simulation step is in range
        //this.pedestrianToMoveInRange.foreach(p => println(p.currentPosition))
        this.moveInRangeStep()
      } else { // else move all pedestrians with the isolation step
        this.pedestrianToMoveInIsolation.foreach(ped => {
          walkPedestrian(ped, getPedInLevelVicinity_3D(ped, sim.population), getClosestCoordinates3D(ped, sim.walls), this.isolatedTimeStepSeconds)
        })
      }

    sim.processCompletedPedestrian(sim.finalDestinationReached)
    sim.population.filter(sim.intermediateDestinationReached).foreach(p => sim.updateIntermediateDestination(p))

    sim.rebuildMTree()

    // enqueues pedestrians in the waiting zones if gating is used
    if (sim.useFlowGates) {
      sim.controlDevices.flowGates.foreach(fg => {
        sim.population
          .filter(
            p => p.nextZone == fg.endVertex &&
              !fg.pedestrianQueue.contains(p) &&
              !p.freedFrom.contains(fg.ID) &&
              isInVertex(fg.startVertex)(p.currentPosition))
          .foreach(p => sim.insertEventWithZeroDelay(new fg.EnqueuePedestrian(p, sim)))
      })
    }

    insertNextEvent()
  }

  def walkPedestrian(ped: PedestrianNOMAD, pedestrians: util.ArrayList[InfluenceAreaReturnPedData], obstacles: util.ArrayList[InfluenceAreaReturnObsData], dt: Double): Unit = {


    /*if (sim.population.exists(ped => !sim.spaceSF.isInsideWalkableArea(ped.currentPosition))) {
      val ped = sim.population.find(ped => !sim.spaceSF.isInsideWalkableArea(ped.currentPosition))
    }*/

    val acc: Vector3d = new Vector3d()


    strayingAccelerationFixedTau(acc, new Vector3d(ped.currentVelocity.X, ped.currentVelocity.Y, 0.0), ped.freeFlowVel, new Vector3d(ped.desiredDirection.X, ped.desiredDirection.Y, 0.0), ped.tau)

    if (pedestrians != null && !pedestrians.isEmpty) {
      pedsRepellingPhysicalAcceleration(
        acc,
        ped,
        new Vector3d(ped.currentVelocity.X, ped.currentVelocity.Y, 0.0),
        pedestrians
      )
    }

    /*infrastructure repulsion */
    if (obstacles != null && !obstacles.isEmpty) {
      acc.add(obstacleRepulsionAndPhysical(ped, new Coordinate(ped.currentPosition.X, ped.currentPosition.Y, 0.0), new Vector3d(ped.desiredDirection.X, ped.desiredDirection.Y, 0.0), obstacles))
    }

    calculateNextPosition(acc, new Vector3d(0.0, 0.0, 0.0), ped, dt, new Coordinate(ped.currentPosition.X, ped.currentPosition.Y), new Vector3d(ped.currentVelocity.X, ped.currentVelocity.Y, 0.0))
  }

  private def moveInRangeStep(): Unit = { // ask the pedestrians in isolation to move
    //this.pedestrianToMoveInIsolation.foreach this.isolatedTimeStepSeconds, currentTime)
    this.pedestrianToMoveInIsolation.foreach(ped => {
      walkPedestrian(ped, getPedInLevelVicinity_3D(ped, sim.population), getClosestCoordinates3D(ped, sim.walls), this.isolatedTimeStepSeconds)
    })
    // for each in range step
    var rangeStep = this.rangeTimeStepSeconds
    var rangeCounter: Int = 0
    while ( {
      rangeCounter <= 5
    }) { // move the pedestrians in queues
      //movePedestriansInQueues(this.rangeTimeStepSeconds, currentTime)
      // ask the in range pedestrians to perform the activity(walking included)
      this.pedestrianToMoveInRange.foreach(ped => {
        walkPedestrian(ped, getPedInLevelVicinity_3D(ped, sim.population), getClosestCoordinates3D(ped, sim.walls), this.rangeTimeStepSeconds)
      })
      //walkPedestrians(this.pedestrianToMoveInRange, this.rangeTimeStepSeconds, currentTime)
      //if (Pedestrian.isParallel) Pedestrian.updateParallel(this.pedestrianToMoveInRange)

      sim.population.foreach(ped => {
        ped.currentPosition = ped.nextPosition
        ped.currentVelocity = ped.nextVelocity
        ped.travelDistance += (ped.currentPosition - ped.getHistoryPosition.last._2).norm
        ped.addHistory(sim.currentTime + Time(rangeStep))
      })
      rangeStep += this.rangeTimeStepSeconds
      rangeCounter += 1
    }
    // after the in range steps finished try to walk the residue
    // of the simulation step
    if (this.remainderInRangeSeconds > 0) {
      //movePedestriansInQueues(this.remainderInRangeSeconds, currentTime)
      this.pedestrianToMoveInRange.foreach(ped => {
        walkPedestrian(ped, getPedInLevelVicinity_3D(ped, sim.population), getClosestCoordinates3D(ped, sim.walls), this.rangeTimeStepSeconds)
      })
      //walkPedestrians(this.pedestrianToMoveInRange, this.remainderInRangeSeconds, currentTime)
      //if (Pedestrian.isParallel) Pedestrian.updateParallel(this.pedestrianToMoveInRange)
    }
    //if (Pedestrian.isParallel) Pedestrian.updateParallel(this.pedestrianToMoveInIsolation)
  }

  private def moveInCollisionStep(): Unit = {

    //println(this.pedestrianToMoveInIsolation.size, this.pedestrianToMoveInRange.size, pedestrianToMoveInCollision.size, sim.population.size)
    var rangeStep = this.rangeTimeStepSeconds
    // ask the pedestrians in isolation to move but do not update their position
    // in case of parallel walking
    this.pedestrianToMoveInIsolation.foreach(ped => {
      walkPedestrian(ped, getPedInLevelVicinity_3D(ped, sim.population), getClosestCoordinates3D(ped, sim.walls), this.isolatedTimeStepSeconds)
    })
    //walkPedestrians(this.pedestrianToMoveInIsolation, this.isolatedTimeStepSeconds, currentTime)
    // for each in collision step
    var colStep = this.collisionTimeStepSeconds
    //var colCounter: Int = 1
    //var rangeCounter: Int = 1
    while ( {
      colStep < this.isolatedTimeStepSeconds //colStep <= this.isolatedTimeStepSeconds
    }) { // move the pedestrians in queues
      //movePedestriansInQueues(this.collisionTimeStepSeconds, currentTime)
      // ask the in collision pedestrians to perform the activity(walking included)
      this.pedestrianToMoveInCollision.foreach(ped => {
        walkPedestrian(ped, getPedInLevelVicinity_3D(ped, sim.population), getClosestCoordinates3D(ped, sim.walls), this.collisionTimeStepSeconds)
      })
      //walkPedestrians(this.pedestrianToMoveInCollision, this.collisionTimeStepSeconds, currentTime)
      // check if the range step is reached
      if (colStep % this.rangeTimeStepSeconds == 0) {
        this.pedestrianToMoveInRange.foreach(ped => {
        })
        //walkPedestrians(this.pedestrianToMoveInRange, this.rangeTimeStepSeconds, currentTime)
        rangeStep += this.rangeTimeStepSeconds
        //println("range step process in collison", rangeStep)
      }
      // if it is a parallel update then update the next position of pedestrians
      /*if (Pedestrian.isParallel) {
        Pedestrian.updateParallel(this.pedestrianToMoveInCollision)
        Pedestrian.updateParallel(this.pedestrianToMoveInRange)
      }*/

      sim.population.foreach(ped => {
          ped.currentPosition = ped.nextPosition
          ped.currentVelocity = ped.nextVelocity
          ped.travelDistance += (ped.currentPosition - ped.getHistoryPosition.last._2).norm
          ped.addHistory(sim.currentTime + Time(colStep))
      })
      colStep += this.collisionTimeStepSeconds
      //colCounter += 1
    }
    // after the in collision steps finished try to walk the residue
    // of the simulation step
    if (this.remainderInCollisionSeconds > 0) {
      //movePedestriansInQueues(this.remainderInCollisionSeconds, currentTime)
      this.pedestrianToMoveInCollision.foreach(ped => {
        walkPedestrian(ped, getPedInLevelVicinity_3D(ped, sim.population), getClosestCoordinates3D(ped, sim.walls), this.collisionTimeStepSeconds)
      })
      //walkPedestrians(this.pedestrianToMoveInCollision, this.remainderInCollisionSeconds, currentTime)
      //if (Pedestrian.isParallel) Pedestrian.updateParallel(this.pedestrianToMoveInCollision)
      // ask the in range to make the last step before the remainder
      if (rangeStep <= this.isolatedTimeStepSeconds) {
        this.pedestrianToMoveInRange.foreach(ped => {
          walkPedestrian(ped, getPedInLevelVicinity_3D(ped, sim.population), getClosestCoordinates3D(ped, sim.walls), this.rangeTimeStepSeconds)
        })
        //walkPedestrians(this.pedestrianToMoveInRange, this.rangeTimeStepSeconds, currentTime)
        //if (Pedestrian.isParallel) Pedestrian.updateParallel(this.pedestrianToMoveInRange)
      }
    }
    if (this.remainderInRangeSeconds > 0) {
      this.pedestrianToMoveInRange.foreach(ped => {
        walkPedestrian(ped, getPedInLevelVicinity_3D(ped, sim.population), getClosestCoordinates3D(ped, sim.walls), this.rangeTimeStepSeconds)
      })
      //walkPedestrians(this.pedestrianToMoveInRange, this.remainderInRangeSeconds, currentTime)
      //if (Pedestrian.isParallel) Pedestrian.updateParallel(this.pedestrianToMoveInRange)
    }
    //if (Pedestrian.isParallel) Pedestrian.updateParallel(this.pedestrianToMoveInIsolation)
  }


  def strayingAccelerationFixedTau(acceleration: Vector3d, speed: Vector3d, v0: Double, desiredDirection: Vector3d, tau: Double): Unit = {
    /*// **** trying to create a varying tau
       double[] speeds = {0.5,0.75,1.0};
       double[] taus = {0.78,0.55,0.25,0.18};
       //double[] taus = {0.18,0.35,0.25,0.18};

       int index = Arrays.binarySearch(speeds, _pedestrian.getSpeed().length());
       index =  (-(index) - 1);*/
    val strayingAcceleration = desiredDirection
    strayingAcceleration.scale(v0)
    // get the difference to the current speed
    strayingAcceleration.sub(speed)
    // divide by the reaction time (tau)
    strayingAcceleration.scale(1 / tau) //CORRECT

    //**** trying to create a varying tau
    //strayingAcceleration.scale(1/taus[index]);// to be used with varying tau
    acceleration.add(strayingAcceleration)
  }

  def pedsRepellingPhysicalAcceleration(pressure: Vector3d, thisPedestrian: PedestrianNOMAD, thisSpeed: Vector3d, pedsDistanceData: util.ArrayList[InfluenceAreaReturnPedData]): Unit = {
    /*
       dx = dx[*,inear]								; Afstandsvector tussen voetgangers
       dpq = dpq[inear]								; Absolute afstand
       npq = dx / (UNIT2 # dpq)						; Genormeerde afstandsvector
       dv = PEDarr[inear].v - UNIT ## PEDarr[pp].v		; Snelheidsverschil
       rpq =  PEDarr[inear].r + UNIT ## PEDarr[pp].r	; Som cirkelstralen
       tpq = [-npq[1,*], npq[0,*]]						; tangential direction
       gpq = ((rpq - dpq) > 0d0)						; 0 indien afstand groter dan de straal, anders gelijk aan het argument*/
    val radius = thisPedestrian.r
    val a0 = thisPedestrian.a0
    val r0 = thisPedestrian.r0
    val a1 = thisPedestrian.a1
    val r1 = thisPedestrian.r1
    val kappa = thisPedestrian.kappa
    val k0 = thisPedestrian.k0
    // for each pedestrian distance data
    import scala.collection.JavaConversions._
    for (pedDistanceData <- pedsDistanceData) { // check if the pedestrians are touching each other
      // (compressing each other)a
      //println("ped interaction")
      if (pedDistanceData.gpq >= 0) { // if they touch then calculate the compressing force in the normal and tangential directions

        pedestrianPhysical(pressure, kappa, thisSpeed, k0, pedDistanceData)

        //println("debug in physical ped, after" + pressure)
        //System.out.println("compressing ped");
      }
      else { // else calculate the conventional repelling force
        // modify the normal acceleration to represent the repelling acceleration due to this pedestrian
        // *** OLD ***
        //pedestrianRepell(acceleration, thisPedestrian, gpq, pedDistanceData);
        // *** NEW ***	// change the InfluenceAnisotropic too
        //println("debug in repell ped, before: " + pressure)
        pedestrianRepellOpposing(pressure, radius, a0, r0, a1, r1, pedDistanceData)
        //println("debug in repell ped, after:" + pressure)

      }
    }
  }

  def obstacleRepulsionAndPhysical(_pedestrian: PedestrianNOMAD, position: Coordinate, speed: Vector3d, obstaclesData: util.ArrayList[InfluenceAreaReturnObsData]): Vector3d = {
    val repulsion = new Vector3d
    // the touching distance of the pedestrians
    var dpw = .0
    var gpw = .0
    val x = position.x
    val y = position.y
    //double z = position.z;
    val radius = _pedestrian.r
    // for each coordinates
    import scala.collection.JavaConversions._
    for (obstacleData <- obstaclesData) {
      /*			xobs = REFORM(PEDonOBS[*,pp,*])
           tobs = reform(tPEDonOBS[*,pp,*])

           sign = UNIT2 # REFORM(Contained[pp,*]) * 2 + 1

           dx = xobs - UNITOBS ## PEDarr[pp].x */
      // get the vector pointing towards the pedestrian (dx -> npw)
      val npW = new Vector3d(obstacleData.coordinate.x - x, obstacleData.coordinate.y - y, 0.0)
      /*			dpW = SQRT(TOTAL(dx * dx,1))	*/ dpw = GeometryUtils.lengthxy(npW)
      //npW = dx / (UNIT2 # dpW)
      //if (dpw != 0)
      GeometryUtils.normalizexy(npW)
      /*			; Aanpassing 31/07/2003
                rpW = UNITOBS # PEDarr[pp].r
                gpW = ((rpW - dpW) > 0d0)*/ gpw = radius - dpw
      // check if the pedestrian and the obstacle is intersecting
      // (compressing each other) pedDistanceNet < 0
      if (gpw > 0) { // if they intersect then calculate the extreme compressing force in the normal and tangential directions
        obstaclePhysical(repulsion, _pedestrian, speed, gpw, npW, obstacleData)
        //System.out.println("compressing ped");
      }
      else { // else calculate the conventional repulsion force
        obstacleRepell(repulsion, _pedestrian, -gpw, npW, obstacleData)
      }
    }
    // convert the repulsion back to this level.
    repulsion //_pedestrian.getLevel().makeVector2D(repulsion);

  }

  def obstaclePhysical(acceleration: Vector3d, _pedestrian: PedestrianNOMAD, speed: Vector3d, gpW: Double, npW: Vector3d, obstacleData: InfluenceAreaReturnObsData): Unit = {
    /*		xobs = REFORM(PEDonOBS[*,pp,*])
       sign = UNIT2 # REFORM(Contained[pp,*]) * 2 + 1

       rpW = UNITOBS # PEDarr[pp].r
       gpW = ((rpW - dpW) > 0d0)
        */
    // *** tangential acceleration
    // get the normalised tangential vector
    //tpW = [-npW[1,*], npW[0,*]]
    val tpW = new Vector3d(-npW.y, npW.x, npW.z)
    // the projection of the difference speed into the tangentialR acceleration
    // OBS: the module of dy is always 1 therefore we do not need to divide the dot product by the module
    //dvtpW = -TOTAL(PEDarr[pp].v * tpW, 1)
    //Ftan = - kappai[pp] * (UNIT2 # (gpW * dvtpW)) * tpW
    tpW.scale(-_pedestrian.kappa * (gpW * GeometryUtils.dotxy(speed, tpW)))
    //		*** normal acceleration
    // correct the normal acceleration
    //StimNear = ki[pp] * gpW
    // add to the normal acceleration
    //Fnorm = - sign * npW * (UNIT2 # (StimFar + StimNear))
    npW.scale(-_pedestrian.k0 * gpW)
    /*// *** total acceleration
        acceleration.add(_pedestrian.getLevel().makeVector2D(npW));
        acceleration.add(_pedestrian.getLevel().makeVector2D(tpW));*/ acceleration.add(npW)
    acceleration.add(tpW)
  }

  def obstacleRepell(acceleration: Vector3d, _pedestrian: PedestrianNOMAD, dpw: Double, npW: Vector3d, obstacleData: InfluenceAreaReturnObsData): Unit = {
    /*
   ; Wijziging 19/04/03
   ; - Bij obstakels geen invloed tenzij obstakel te dichtbij.
   ; - Eventueel in later stadium effect aanpassen voor andere oppervlakken
       odist0 = 0.3	;	afstand tot obstakels in meters
       StimFar = AiW[pp] * (((1.0 - (dpW - odist0) / (odist0 + 1.0 * (odist0 eq 0))) > 0.0) < 1.0)*/
    // M: (1.0 - (dpW - odist0) / odist0) > 0.0 => dpW - rpW < 2*odist0
    // M: (1.0 - (dpW - odist0) / odist0) < 1.0 => dpW - rpW  > odist0
    var factor = 1 - (dpw - 0.2 /*NOMAD.defaults.ROUTE_CHOICE_MAX_OBSTACLE_DISTANCE*/) / 0.2 //NOMAD.defaults.ROUTE_CHOICE_MAX_OBSTACLE_DISTANCE
    if (factor >= 1) factor = 1
    // check if the pedestrian is not too close or too far from the obstacle
    if (factor > 0) {
      npW.scale(-obstacleData.obstacle.aw * factor)
      // add to the normal acceleration
      //acceleration.add(_pedestrian.getLevel().makeVector2D(npW));
      acceleration.add(npW)
    }
    // else do nothing
  }

  /*
  def movePedestrians(currentTime: Time): Unit = { // if it is variable step
    if (Pedestrian.isVariableStep) { // get the minimum step
      // it can be that that there is no pedestrian in collision
      //minTimeStep = this.getSmallestTimeStep();
      // if there is at least one pedestrian inCollision then the smallest
      // variable step is a in collision step.
      if (!this.pedestrianToMoveInCollision.isEmpty) this.moveInCollisionStep(currentTime)
      else { // check the in range step
        if (!this.pedestrianToMoveInRange.isEmpty) { // if at least one is in range and the rest is at isolation or in range
          // then the smallest simulation step is in range
          this.moveInRangeStep(currentTime)
        }
        else { // else move all pedestrians with the isolation step
          this.moveInIsolationStep(currentTime)
        }
      }
    }
    else { // if it is not a variable time step
      // move the pedestrians in queues
      //movePedestriansInQueues(this.isolatedTimeStepSeconds, currentTime)
      // move all pedestrians according to the simulation step
      //walkPedestrians(this.pedestrianToMoveInCollision, this.isolatedTimeStepSeconds, currentTime)
      // if it is a parallel update then update the next position of pedestrians
      //if (Pedestrian.isParallel) Pedestrian.updateParallel(this.pedestrianToMoveInCollision)
    }
    Pedestrian.updateAccelerations(this.pedestrianToMoveInCollision, NomadModel.model.simTime.getTimeStepSeconds)
  }*/


  def getPedInLevelVicinity_3D_Ani(thisPedestrian: PedestrianNOMAD, pedestrians: Iterable[PedestrianNOMAD] /*, obstacles: util.ArrayList[InfrastructureObject]*/): util.ArrayList[InfluenceAreaReturnPedData] = {
    /** the vector pointing from the pedestrian to his neighbour */
    val dx = new Vector3d
    /** the projection of dx to the direction of the speed vector of the pedestrian */
    var dxAlongEp = .0
    /** the dx component in the direction of the speed vector of the pedestrian */
    val dxn = new Vector3d
    /** the dx component in the perpendicular direction of the speed vector of the pedestrian */
    val dxt = new Vector3d
    /** the perceived distance */
    var dpq = .0
    /** flag that tells if the other pedestrian is in front of back */
    var front = false
    /** tells the direction of the speed of the pedestrian that is being perceived.
      * negative values mean that pedestrians are walking in opposing directions.
      * zero means that they are walking perpendicular to each other. */
    var vDir = .0
    /** Specifies the use of full precision. if false, pedestrians are not checked
      * if behind obstacles (this has a huge impact in computational performance).
      * default is true. */
    val isPrecise = true


    def setPedestriansData(thisX: Double, thisY: Double, thisVx: Double, thisVy: Double, thisRadius: Double, thisIeb: Double, thisIef: Double, thisAT: Double, thisC0min: Double, thisC0plus: Double, otherX: Double, otherY: Double, otherVx: Double, otherVy: Double, otherRadius: Double, //, ArrayList<InfrastructureObject> obstacles,
                           otherPedestrian: PedestrianNOMAD, tempList: util.ArrayList[InfluenceAreaReturnPedData]): Unit = {
      val serialVersionUID = 1284024937843891575L

      var extension = 0.0
      // check if there is a obstacle in between
      var intersectCheck = false
      val otherSpeed = new Vector3d(otherVx, otherVy, 0)
      val distance = GeometryUtils.lengthxy(dx)


      // Immediately calculates if they are colliding
      val gpq = thisRadius + otherRadius - distance
      // if they are not colliding
      if (gpq < 0) { // **** calculate the dpq and the rest of the parameters
        // calculate the vector unit of the speed of this pedestrian
        // dxn = (Vector3d) _pedestrian.getCurrentSpeed().clone(); // NOT TO USE super slow
        dxn.x = thisVx
        dxn.y = thisVy
        //dxn.z = thisPedestrian.getSpeed().z;
        GeometryUtils.normalizexy(dxn)
        // get the projection of the vector distance between the pedestrian against the speed of this pedestrian
        dxAlongEp = GeometryUtils.dotxy(dx, dxn)
        // ***** NEW BEHAVIOUR ****** //
        // get the projection between the speeds
        vDir = GeometryUtils.dotxy(otherSpeed, dxn)
        // OLD!! the anticipation is only for pedestrians q walking in front and against of pedestrian p
        //if (dxAlongEp>0 && vDir<=0 && (dx.length() - thisPedestrian.getRadius() - otherPedestrian.getRadius())>0){
        // NEW!! anticipation to any pedestrian walking in front of this pedestrian
        if (dxAlongEp > 0 && (distance - thisRadius - otherRadius) > 0) {
          val tempDx = new Vector3d
          tempDx.x = otherX + otherVx * thisAT - thisX - thisVx * thisAT
          tempDx.y = otherY + otherVy * thisAT - thisY - thisVy * thisAT
          if (tempDx.x > 100  || tempDx.y > 100) {
            println("stop")
          }
          //tempDx.z = 0.0;
          // check if the anticipated position did not switch and ended behind the other pedestrian
          val tempDxAlonpEp = GeometryUtils.dotxy(tempDx, dxn)
          if (tempDxAlonpEp > 0) {
            dxAlongEp = tempDxAlonpEp
            dx.x = tempDx.x
            dx.y = tempDx.y
            //dx.z = tempDx.z;
          }
        }
        // ***** END OF NEW BEHAVIOUR ****** //
        // scale dxn
        dxn.scale(dxAlongEp)
        // calculate dxt
        dxt.x = dx.x
        dxt.y = dx.y
        //dxt.z = dx.z;
        dxt.sub(dxn)
        // calculate the dpq
        // check if the pedestrian is strictly in the the back of the pedestrian
        if (dxAlongEp < 0) { // pedestrian is in the back of _pedestrian (c0Min)
          // the c0min is already squared
          dpq = Math.sqrt(thisC0min * GeometryUtils.lengthSquaredxy(dxn) + GeometryUtils.lengthSquaredxy(dxt))
          extension = thisIeb
          front = false
        }
        else { // pedestrian is in the front of _pedestrian (c0Plus) or his speed is zero
          // the c0plus is already squared
          dpq = Math.sqrt(thisC0plus * GeometryUtils.lengthSquaredxy(dxn) + GeometryUtils.lengthSquaredxy(dxt))
          extension = thisIef
          front = true
        }
        // **** debugging *******//
        //System.out.println("dpq: " + dpq + "  |  dx.length(): " + dx.length());
        // **** debugging *******//
        // normalize dx
        dx.scale(1.0 / distance)
        intersectCheck = true //isPrecise && !JTSUtils.intersects3D(obstacles, new Coordinate(thisX,thisY),  new Coordinate(otherX,otherY));

        // NEED TO DEAL CORRECLY WITH OBSTACLES NM
        // check if dpq is smaller then the max extent and
        // if the pedestrian is not behind an obstacle and therefore not visible to _pedestrian
        if (dpq <= extension//!! ATTENTION should be present!!!
          // BUT has an enormous impact in the performance
          ) { // if yes then add him and the necessary data to the return list
          tempList.add(new InfluenceAreaReturnPedData(null, otherSpeed, otherRadius, dx, dxn, dxt, dpq, gpq, front, vDir))
        }
      }
      else { // if the gpq is non zero (colliding) then add him and the necessary data to the return list
        tempList.add(new InfluenceAreaReturnPedData(null, otherSpeed, otherRadius, dx, dxn, dxt, dpq, gpq, front, vDir))
      }
    }


    val thisRadius = thisPedestrian.r
    val tempList = new util.ArrayList[InfluenceAreaReturnPedData]
    val thisIef = thisPedestrian.ief
    val thisIeb = thisPedestrian.ieb
    val this3DPosition = new Coordinate(thisPedestrian.currentPosition.X, thisPedestrian.currentPosition.Y, 0.0)
    val thisX = this3DPosition.x
    val thisY = this3DPosition.y
    val this3DSpeed = new Vector3d()
    this3DSpeed.set(thisPedestrian.currentVelocity.X, thisPedestrian.currentVelocity.Y, 0.0)
    val thisVx = this3DSpeed.x
    val thisVy = this3DSpeed.y
    val thisC0min = thisPedestrian.c0min
    val thisC0plus = thisPedestrian.c0plus
    val thisId = thisPedestrian.ID
    val thisAT = thisPedestrian.AT
    val maxDist = Math.max(thisIef, thisIeb) + thisRadius
    //*** A crude version NOT VALIDATED of a speed bias to the right
    /*		double theta = -Math.PI/10;
           //xN = x*cos(theta) - y*sin(theta);
           //yN = x*sin(theta) - y*cos(theta);
          double thisVxN = thisVx*Math.cos(theta) - thisVy*Math.sin(theta);
          double thisVyN = thisVx*Math.sin(theta) + thisVy*Math.cos(theta);
          thisVx = thisVxN;
          thisVy = thisVyN;*/
    //*** END of speed bias
    // for each pedestrian from the list

    for (otherPedestrian <- pedestrians) { //  check if the pedestrians are different.
      if (otherPedestrian.ID != thisId) { //*** initialisation
        /*val dx = new Vector3d
          val dxn = new Vector3d
          val dxt = new Vector3d*/
        val other3DPosition = new Coordinate(otherPedestrian.currentPosition.X, otherPedestrian.currentPosition.Y, 0.0)
        val otherX = other3DPosition.x
        val otherY = other3DPosition.y
        val other3DSpeed = new Vector3d()
        other3DSpeed.set(otherPedestrian.currentVelocity.X, otherPedestrian.currentVelocity.Y, 0.0)
        val otherVx = other3DSpeed.x
        val otherVy = other3DSpeed.y
        val otherRadius = otherPedestrian.r
        //Vector3d otherSpeed = otherPedestrian.getSpeed();
        //**** calculation
        // calculate the vector position difference between the pedestrians
        dx.x = otherX - thisX
        dx.y = otherY - thisY
        //dx.z = otherPedestrian.getPosition().z - thisPedestrian.getPosition().z;
        // make a simple check of how far are the pedestrians
        // only try to calculate the perceived distance if both the coordinates are at least
        // equal to the maximum extension of the influence area
        if (Math.abs(dx.x) <= maxDist + otherRadius && Math.abs(dx.y) <= maxDist + otherRadius) {
          setPedestriansData(
            thisX, thisY,
            thisVx, thisVy,
            thisRadius,
            thisIeb, thisIef,
            thisAT,
            thisC0min, thisC0plus,
            otherX, otherY,
            otherVx, otherVy,
            otherRadius, /*obstacles,*/
            otherPedestrian,
            tempList)
        }
      }
    }
    tempList
  }


  def getClosestCoordinates3D(pedestrian: PedestrianNOMAD, obstacles: Iterable[Wall]): util.ArrayList[InfluenceAreaReturnObsData] = { // it has to be with the global coordinate because of the change of levels
    val coordinate = new Coordinate(pedestrian.currentPosition.X, pedestrian.currentPosition.Y, 0.0)
    val coordinatesInVicinity = new util.ArrayList[InfluenceAreaReturnObsData]
    var closestPoint: Coordinate = null
    val radius = pedestrian.r
    val maxExtension = pedestrian.infAreaMaxExtObs
    // for each obstacle
    for (obstacle <- obstacles) { // only check this object if it is repealing
      //if (obstacle.asInstanceOf[NomadObstacle].isRepel) { // get the closest point // SKIPPED THIS BECAUSE WANT TO SIMPLIFY THINGS NM
      closestPoint = new Coordinate(getClosestPoint(Vector2D(coordinate.x, coordinate.y), obstacle).X, getClosestPoint(Vector2D(coordinate.x, coordinate.y), obstacle).Y)
      // check if the closest point of a obstacle is behind another obstacle
      // if yes it will not be added to the return list.
      // ATTENTION! it can be that there would be other points visible to the pedestrian
      // that should be taken instead. This would require an iterative process to cover this possibility.
      // We do not check this because in this case the object that is in front must have a closest point
      // that will be more influential for the repel forces. Although this closest point
      // may not lie over the exact line that connects the refused closest point and the pedestrian.
      if (closestPoint.distance(coordinate) <= 15 && closestPoint.distance(coordinate) - radius <= maxExtension) { // convert back the closest point to the local coordinates
        //closestPoint = pedestrian.getLevel().makeCoordinate2D(closestPoint);
        coordinatesInVicinity.add(new InfluenceAreaReturnObsData(obstacle, closestPoint))
      }
      //}
    }
    coordinatesInVicinity
  }


  def calculateNextPosition(acceleration: Vector3d, pressure: Vector3d, pedestrian: PedestrianNOMAD, /*obstaclesInVicinity: util.ArrayList[InfluenceAreaReturnObsData],*/ dt: Double, position: Coordinate, speed: Vector3d): Unit = { //-------- set the dynamic colours of pedestrians ----------------//

    //Pedestrian.setDynamicColour(pedestrian, acceleration, pressure)
    //-------- add the accelerations ---------------//
    acceleration.x += pressure.x
    acceleration.y += pressure.y
    //-------- add the noise ---------------//
    if (pedestrian.isStochastic) { // the noise is dependent of dt to make sure that it will be the same regardless of
      // the number of intermediate steps that the pedestrians are doing. This is only valid
      // for the variable time step. However for the same simulation different using different
      // time steps will introduce different stochasticity. Therefore we consider the noise input
      // as noise/second.
      acceleration.x += calculateNoise(dt, pedestrian.noise)
      acceleration.y += calculateNoise(dt, pedestrian.noise)
    }
    //-------- limit the acceleration version1---------------//
    var adjustingFactor: Double = 1.0
    // check if the acceleration is smaller then the max acceleration factor
    // if the normal acceleration is bigger then max
    if (GeometryUtils.lengthxy(acceleration) > 5.0) { // NomadModel.model.simParametersXML.getRunTimePars.getAccelMax) { // adjust the acceleration
      adjustingFactor = 5.0 / GeometryUtils.lengthxy(acceleration); // NomadModel.model.simParametersXML.getRunTimePars.getAccelMax / GeometryUtils.lengthxy(acceleration)
      acceleration.scale(adjustingFactor)
    }

    pedestrian.acceleration = Vector2D(acceleration.x, acceleration.y)
    //** calculate the speed variation
    // dv1 * dt
    acceleration.scale(dt)
    // make the next speed
    val nextSpeed = new Vector3d(speed)
    //; Voer eerst de update van de snelheid en de plaats uit:
    //PEDarr.v = PEDarr.v + dv1 * dt
    nextSpeed.add(acceleration)
    // adjust the speed and acceleration if necessary
    if (GeometryUtils.lengthxy(nextSpeed) > 2.0) {
      // 2*(v0+s0) pedestrian.getType.getMaxSpeed) {
      adjustingFactor = 2.0 / GeometryUtils.lengthxy(nextSpeed)
      // adjust the speed
      nextSpeed.scale(adjustingFactor)
      acceleration.sub(nextSpeed, speed)
    }
    // **** end of V1
    //-------- calculate the new position and the new speed ---------------//
    //** calculate the position **//
    // PEDarr.x + v1 * dt + 0.5 * dv1 * dt * dt

    pedestrian.nextVelocity = Vector2D(nextSpeed.x, nextSpeed.y)
    pedestrian.nextPosition = Vector2D(position.x, position.y) + Vector2D(nextSpeed.x * dt, nextSpeed.y * dt)


    /*if (Pedestrian.isParallel) { // if it is parallel update
      // use the next speed and next position of the pedestrian
      // that will be updated only after the current step
      if (Pedestrian.isNextSpeed) { // if it is the time integration using the new speed (next speed)
        nextCoordinate = new Coordinate(position.x + nextSpeed.x * dt, position.y + nextSpeed.y * dt, position.z)
      }
      else { // use the time integration using the average speed
        nextCoordinate = new Coordinate(position.x + (speed.x + acceleration.x * 0.5) * dt, position.y + (speed.y + acceleration.y * 0.5) * dt, position.z)
      }
    }
    else {*/
    // if the update is sequential then immediately update the position (and speed)
    /*if (Pedestrian.isNextSpeed) */
    //nextCoordinate = new Coordinate(position.x + nextSpeed.x * dt, position.y + nextSpeed.y * dt, position.z)
    //else nextCoordinate = new Coordinate(position.x + (speed.x + acceleration.x * 0.5) * dt, position.y + (speed.y + acceleration.y * 0.5) * dt, position.z)
    //}
    //new WalkingStateStructure(pedestrian, nextCoordinate, nextSpeed)
  }


  protected def calculateNoise(dt: Double, noise: Double): Double = { // the regression formula for devRatios >> 10
    // the regression formula log(devRatio) = -0.486 * log(dtRatio) + 0.4305
    //double devRatio = Math.exp(-0.486*Math.log(dtRatio) + 0.4305);
    // the regression formula for devRatios < 10
    // y = 0.0056dtRatio^2 - 0.01147dtRatio + 1.1091
    val dtRatio = 0.1 / dt // NomadModel.model.simTime.getTimeStepSeconds / dt
  val devRatio = 0.0056 * Math.pow(dtRatio, 2) - 0.1147 * dtRatio + 1.1091
    ThreadLocalRandom.current.nextGaussian() * noise * devRatio
    //NomadRandom.nextDoubleNormal(0, noise * devRatio)
  }


  protected def insertNextEvent(): Unit = sim.insertEventWithDelay(Time(this.isolatedTimeStepSeconds))(new NOMADIntegrated(sim))


  def getPedInLevelVicinity_3D(thisPedestrian: PedestrianNOMAD, pedestrians: Iterable[PedestrianNOMAD]/*, obstacles: java.util.ArrayList[InfrastructureObject]*/): java.util.ArrayList[InfluenceAreaReturnPedData] = {

    val tempList = new java.util.ArrayList[InfluenceAreaReturnPedData]
    // for each pedestrian from the list
    for (otherPedestrian <- pedestrians) { //  check if the pedestrians are different.
      //if (otherPedestrian.getName().compareToIgnoreCase(thisPedestrian.getName())!=0){
      if (otherPedestrian.ID != thisPedestrian.ID) {
        /*			Serge
              dx = PEDarr.x - UNIT0 ## PEDarr[pp].x		; verschil vector tussen de voetgangers (bruto) PEDarr[pp] -> pedestrian p
             rpq =  PEDarr.r + UNIT0 ## PEDarr[pp].r	    ; som van de stralen
             dpq = (SQRT(TOTAL(dx * dx, 1)))			    ; bruto afstand voetganger p en q
             spq = (dpq - rpq) > 0D0						; netto afstand voetganger p en q

             ; Bepaal componenten van dx in longitudinale en laterale richting
             dx_along_ep = TOTAL(dx * (UNIT0 ## ep[*,pp]),1)		; verschil vector langs ep (genormeerde snelheidsrichting)
             dxn = (UNIT0 ## ep[*,pp]) * (UNIT2 # dx_along_ep) ; dx scale corrected
             dxt = (dx - dxn)																	; the dx in the tangential direction

             ; Bepaal of q voor of achter p zit GT -> greater then  LE -> less equal then
             signpq = 1D0 * (dx_along_ep GT 0D0) - 1D0 * (dx_along_ep LE 0D0)

             ; Bepaal de correcte waarde van cpq
             cpq = cplus[pp] * (signpq GT 0D0) + cmin[pp] * (signpq LE 0D0)

             ; Wijziging per 20/06/2001: toevoegen anisotropy e.d.
             ; Bepaal per interacterende voetganger de cpq
             dpq = SQRT(cpq * cpq * TOTAL(dxn * dxn,1) + TOTAL(dxt * dxt,1))*/
        //				calculate the dx
        val dx = new Vector3d
        dx.x = otherPedestrian.currentPosition.X - thisPedestrian.currentPosition.X
        dx.y = otherPedestrian.currentPosition.Y - thisPedestrian.currentPosition.Y
        //dx.z = otherPedestrian.getPosition().z - thisPedestrian.getPosition().z;
        //	dx.sub(pedestrian.getPosition3d(),_pedestrian.getPosition3d());
        val dpq = GeometryUtils.lengthxy(dx)
        // Immediately calculates if they are colliding
        val gpq = thisPedestrian.getRadius + otherPedestrian.getRadius - GeometryUtils.lengthxy(dx)
        // check if dpq is smaller then the max extent and
        // if the pedestrian is not behind an obstacle and therefore not visible to _pedestrian
        if (dpq < Math.max(0.0, Math.max(thisPedestrian.ief + 3 * 0.0, thisPedestrian.ieb + 3 * 0.0))) { //	&& !JTSUtils.intersects(obstacles, otherPedestrian.getPosition(), thisPedestrian.getPosition())
          // if yes then add him and the necessary data to the return list
          // *** these are not necessary for the isotropic influence area but they are needed to make
          // the influence area general
          //					calculate dxAlongEp	the projection of dx in the speed vector
          val dxn = new Vector3d
          val dxt = new Vector3d
          dxn.x = thisPedestrian.currentVelocity.X
          dxn.y = thisPedestrian.currentVelocity.Y
          //this.dxn.z = thisPedestrian.getSpeed().z;
          // if dxn is not zero then normalise to make dxn be ep[]
          //if (GeometryUtils.lengthxy(dxn) != 0)
          GeometryUtils.normalizexy(dxn)
          val dxAlongEp = GeometryUtils.dotxy(dx, dxn)
          //					scale dxn
          dxn.scale(dxAlongEp)
          dxt.x = dx.x
          dxt.y = dx.y
          //this.dxt.z = dx.z;
          dxt.sub(dxn)
          // *** these are not necessary for the isotropic influence area
          tempList.add(new InfluenceAreaReturnPedData(null, new Vector3d(otherPedestrian.currentVelocity.X, otherPedestrian.currentVelocity.Y, 0.0), otherPedestrian.getRadius, dx, dxn, dxt, dpq, gpq, false, -1))
        }
      }
    }
    tempList
  }
}
