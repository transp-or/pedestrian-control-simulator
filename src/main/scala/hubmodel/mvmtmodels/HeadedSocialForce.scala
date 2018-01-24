package hubmodel.mvmtmodels

import breeze.linalg.{DenseMatrix, DenseVector, norm}
import breeze.numerics.{cos, sin, sqrt}
import hubmodel.{Acceleration, Direction, Force, PedestrianDES, PedestrianSim, Position, SFGraphSimulator, Velocity}

import scala.collection.mutable.ArrayBuffer
import scala.math.pow

/*
class HeadedSocialForce(sim: SFGraphSimulator) extends SocialForce(sim) {

  override def execute(): Unit = {

    /*def collectForces(p: PedestrianSim): Force = {
      val dir = computeDirection(p.currentPosition, p.currentDestination)
      //println(collectPedestrianRepulsionForces(p, sim.population, breeze.linalg.DenseVector(0.0,0.0)))
      //println("acc: " + computeAcceleration(p.currentVelocity, dir * p.freeFlowVel ), p.currentVelocity, dir, p.freeFlowVel)
      //println("wall: " + collectWallRepulsionForces(p, sim.infraSF.walls, breeze.linalg.DenseVector(0.0,0.0)))
      //println("ped: " + collectPedestrianRepulsionForces(p, sim.population, breeze.linalg.DenseVector(0.0,0.0)))
      computeAcceleration(p.currentVelocity, dir * p.freeFlowVel) + collectWallRepulsionForces(p, sim.infraSF.walls, breeze.linalg.DenseVector(0.0, 0.0)) + collectPedestrianRepulsionForces(p, sim.population.filterNot(ped => norm(p.currentPosition - ped.currentPosition) > 15.0), breeze.linalg.DenseVector(0.0, 0.0))
    }*/



    /*def collectAttractiveForce(p: PedestrianSim): Force = {
      computeAcceleration(p.currentVelocity, currentDirection * p.freeFlowVel)
    }

    def collectRepulsiveForces(p: PedestrianSim): Force = {
      collectWallRepulsionForces(p, sim.infraSF.walls, breeze.linalg.DenseVector(0.0, 0.0)) +
        collectPedestrianRepulsionForces(p, sim.population.filterNot(ped => norm(p.currentPosition - ped.currentPosition) > 15.0), breeze.linalg.DenseVector(0.0, 0.0))
    }*/

    def computePedestrianIncrements(p: PedestrianSim): (Position, Velocity, DenseVector[Double]) = {
      val currentDirection = computeDirection(p.currentPosition, p.currentDestination)

      def collectAttractiveForce(p: PedestrianSim): Force = {
        computeAcceleration(p.currentVelocity, currentDirection * p.freeFlowVel)
      }

      def collectRepulsiveForces(p: PedestrianSim): Force = {
        collectWallRepulsionForces(p, sim.infraSF.walls, breeze.linalg.DenseVector(0.0, 0.0)) +
          collectPedestrianRepulsionForces(p, sim.population.filterNot(ped => norm(p.currentPosition - ped.currentPosition) > 15.0), breeze.linalg.DenseVector(0.0, 0.0))
      }
      /*def collectAttractive(p: PedestrianSim): Force = {
        val dir = computeDirection(p.currentPosition, p.currentDestination)
        computeAcceleration(p.currentVelocity, dir * p.freeFlowVel)
      }*/

      /*def collectRepsulsives(p: PedestrianSim): Force = {
        val dir = computeDirection(p.currentPosition, p.currentDestination)
        collectWallRepulsionForces(p, sim.infraSF.walls, breeze.linalg.DenseVector(0.0, 0.0)) + collectPedestrianRepulsionForces(p, sim.population.filterNot(ped => norm(p.currentPosition - ped.currentPosition) > 15.0), breeze.linalg.DenseVector(0.0, 0.0))
      }*/

      if (!p.isWaiting) {

        //val currentDirection: Direction = p.currentVelocity / breeze.linalg.norm(p.currentVelocity)
        val desiredDirection: Direction = computeDirection(p.currentPosition, p.currentDestination)
        val currentHeadingAngle: Double = p.currentVelocity.dot(DenseVector(1.0, 0.0)) / breeze.linalg.norm(p.currentVelocity)
        //println(currentHeadingAngle)
        val rotationMatix: (Direction, Direction) = (DenseVector(cos(currentHeadingAngle), sin(currentHeadingAngle)), DenseVector(-sin(currentHeadingAngle), cos(currentHeadingAngle)))
        val rotationMatrixBreeze: DenseMatrix[Double] = DenseMatrix((cos(currentHeadingAngle), -sin(currentHeadingAngle)),(sin(currentHeadingAngle), cos(currentHeadingAngle)))
        val rotationMatrixBreezeInverse: DenseMatrix[Double] = DenseMatrix((cos(currentHeadingAngle), -sin(currentHeadingAngle)),(sin(currentHeadingAngle), cos(currentHeadingAngle))).t

        val rotationMatixInverse: (Direction, Direction) = (DenseVector(cos(currentHeadingAngle), -sin(currentHeadingAngle)), DenseVector(sin(currentHeadingAngle), cos(currentHeadingAngle)))

        val forcesRepul: Force = collectRepulsiveForces(p)
        val forcesAttrac: Force = collectAttractiveForce(p)

        val forces: Force = forcesRepul + forcesAttrac
        //println(forces)
        val localForwardForce: Double = forces.dot(rotationMatix._1)
        //println(localForwardForce)
        val k0: Double = 1.0
        val kd: Double = 500.0
        val localOrthogonalForce: Double = k0 * forcesRepul.dot(rotationMatix._2) - kd * rotationMatix._2.dot(p.currentVelocity)
        //println(localOrthogonalForce)

        val klambda: Double = 0.3
        val ktheta: Double = klambda * breeze.linalg.norm(forcesAttrac)
        //println(ktheta)
        val alpha: Double = 3.0
        val komega: Double = (1.0 + alpha) * sqrt((klambda * breeze.linalg.norm(forcesAttrac)) / alpha)
        //println(komega)
        val attractiveForceHeadingAngle: Double = forcesAttrac.dot(DenseVector(1.0, 0.0)) / breeze.linalg.norm(forcesAttrac)
        //println(attractiveForceHeadingAngle)
        val torque: Double = -ktheta * (currentHeadingAngle-attractiveForceHeadingAngle) - komega * p.omega
       // println(torque)

        // set travel time

        p.travelTime = sim.currentTime - p.entryTime

        // collect forces
        // val forces = collectAttractiveForce(p) + collectRepulsiveForces(p)

        // update pos
        //println(forces)
        val acc: Acceleration = rotationMatrixBreeze*DenseVector(localForwardForce, localOrthogonalForce)/p.m
        val posIncr: Position =  p.currentVelocity * sim.dt.toDouble// +  0.5 * pow(sim.dt.toDouble, 2) * acc
        //println(posIncr, p.currentVelocity,dt.toDouble)
        //p.currentPosition = p.currentPosition + posIncr

        // update vel
        val velIncr: Velocity = sim.dt.toDouble * acc
        //p.currentVelocity = boundVelocity(p.currentVelocity + velIncr)

        val rotIncr: DenseVector[Double] = DenseVector(sim.dt.toDouble*p.omega, sim.dt.toDouble*torque)

        // add new position to history
        //p.addHistory(sim.currentTime, p.currentPosition)
        //println((posIncr, velIncr))

        (posIncr, velIncr, rotIncr)
      } else {
        (DenseVector(0.0, 0.0), DenseVector(0.0, 0.0), DenseVector(0.0, 0.0))
      }
    }


    def movePedestrianWithIncrements(l: ArrayBuffer[(PedestrianSim, (Position, Velocity, DenseVector[Double]))]): Unit = {
      l.foreach(pair => {
        pair._1.moveHeaded(pair._2._1, pair._2._2, pair._2._3)
        pair._1.addHistory(sim.currentTime)
      })
    }

    def intermediateDestinationReached: PedestrianSim => Boolean = p => sim.infraGraph.isInZone(p.currentPosition, p.nextZone.name)

    def finalDestinationReached: PedestrianSim => Boolean = p => sim.infraGraph.isInZone(p.currentPosition, p.dZone.toString)


    //println("-- event: time: " + sim.currentTime + ": moving pedestrians")
    //sim.population += ((sim.currentTime*1.5, 2.0)) // adds element
    val pedIncrements: ArrayBuffer[(PedestrianSim, (Position, Velocity, DenseVector[Double]))] = sim.population.zip(sim.population.map(ped => computePedestrianIncrements(ped))) // transformation of mutable val
    //println(pedIncrements)
    movePedestrianWithIncrements(pedIncrements)

    val pedestrianReachedDestination = sim.population.filter(finalDestinationReached)
    sim.populationCompleted ++= pedestrianReachedDestination
    sim.population --= pedestrianReachedDestination
    if (sim.useFlowGates) sim.population.foreach(sim.infraGraph.graph.enqueueInWaitingZone)
    sim.population.filter(intermediateDestinationReached).filterNot(_.isWaiting).foreach(p => sim.insertEventWithDelay(0) {
      new UpdateRouteForPedestrian(p, sim)
    })
    sim.insertEventWithDelay(sim.dt) {
      new HeadedSocialForce(sim)
    }
  }
}

*/