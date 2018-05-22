package nomad.operational;

import java.util.ArrayList;

import nl.tudelft.pedestrians.agents.Pedestrian;
import nl.tudelft.pedestrians.infrastructures.Server;
//import nl.tudelft.pedestrians.main.NomadModel;
import nl.tudelft.pedestrians.main.SimulationTime;
import nl.tudelft.pedestrians.parameters.NOMAD;

public class MovePedestriansWithNOMAD {



    /** the time step for pedestrians in collision in milliseconds */
    private int collisionTimeStepMillis;
    private double collisionTimeStepSeconds;
    /** the time step for pedestrians in in range in milliseconds */
    private int rangeTimeStepMillis;
    private double rangeTimeStepSeconds;
    /** the time step for pedestrians in isolation in milliseconds.
     * This step is equal to the simulation step in milliseconds. */
    private int isolatedTimeStepMillis;
    private double isolatedTimeStepSeconds;

    /** The amount of in range steps in one simulation step */
    //private int inRangeSteps;
    /** The value of the last (remainder) range step in a simulation (in milliseconds)
     *  in case the in range step is not an exact fraction of the simulation step */
    private int remainderInRangeMillis;
    private double remainderInRangeSeconds;
    /** The amount of in collision steps in one simulation step */
    //int inCollisionSteps;
    /** The value of the last (remainder) collision step in a simulation  (in milliseconds)
     *  in case the in collision step is not an exact fraction of the simulation step */
    private int remainderInCollisionMillis;
    private double remainderInCollisionSeconds;


    /**List with pedestrians that are in isolation in the current simulation step*/
    private ArrayList<Pedestrian> pedestrianToMoveInIsolation;
    /**List with pedestrians that are in range in the current simulation step*/
    private ArrayList<Pedestrian> pedestrianToMoveInRange;
    /**List with pedestrians that are in collision in the current simulation step*/
    private ArrayList<Pedestrian> pedestrianToMoveInCollision;


    private double timeStep;
    public MovePedestriansWithNOMAD(int CurrentTime, int timeStep) {

        this.timeStep = timeStep;

        // initialise the in range and in collision time schedule
        this.isolatedTimeStepMillis = timeStep;
        this.isolatedTimeStepSeconds = timeStep / 1000.0;

        this.rangeTimeStepMillis = Math.round(this.isolatedTimeStepMillis*NOMAD.defaults.IN_RANGE_FRACTION);
        this.rangeTimeStepSeconds = SimulationTime.convertSimTime(this.rangeTimeStepMillis);
        //this.inRangeSteps = (int) Math.floor(this.isolatedTimeStepMillis/this.rangeTimeStepMillis);
        this.remainderInRangeMillis = this.isolatedTimeStepMillis%this.rangeTimeStepMillis;
        this.remainderInRangeSeconds = SimulationTime.convertSimTime(this.remainderInRangeMillis);

        this.collisionTimeStepMillis = Math.round(this.isolatedTimeStepMillis*NOMAD.defaults.IN_COLLISION_FRACTION);
        this.collisionTimeStepSeconds = SimulationTime.convertSimTime(this.collisionTimeStepMillis);
        this.remainderInCollisionMillis = this.isolatedTimeStepMillis%this.collisionTimeStepMillis;
        this.remainderInCollisionSeconds = SimulationTime.convertSimTime(this.remainderInCollisionMillis);

    }

    public void step(int currentTime){

        //System.out.println(currentTime);
        // clear the original colour list
        //pedestriansToChangeColour.clear();

        // ask the congestion manager to step
        //congestionManager.step();

        // ask the pedestrians to step
        for (Pedestrian pedestrian: this.pedestriansInSimulation) {

/*			if (pedestrian.getSpeed().length()<0.2)
				System.out.println(pedestrian.pedId);*/

            // update all states from each pedestrian
            pedestrian.step(currentTime);
            // if the pedestrian is not going to exit
            if (!pedestrian.isToExit()){
                // then put him in one of the walking lists
                this.insertPedInMoveList(pedestrian);
            }
            else
                // put him in the exit list
                this.pedestriansToExit.add(pedestrian);
        }

        // ask the appropriate graphic objects to change the pedestrians colours
        //if (Pedestrian.isDynamicColour())this.changePedestriansColours(true);

        // move the pedestrians
        this.movePedestrians(currentTime);

        // Remove the pedestrian from all the external lists after the pedestrians updated their state.
        // This prevents conflict in the for (Pedestrian pedestrian: this.pedestriansInSimulation)
        // that will occur during the step and cause error if a pedestrian is excluded during it.
        for (Pedestrian pedestrian: this.pedestriansToExit){
            this.exitPedestrianFromSimulation(pedestrian, currentTime);
        }

        // clear the lists for the next step
        this.clearLists();

        // once in a while compact the global lists
        //this.compactLists();
    }

    public void movePedestrians(int currentTime) {

        // if it is variable step
        if (Pedestrian.isVariableStep()){
            // get the minimum step
            // it can be that that there is no pedestrian in collision
            //minTimeStep = this.getSmallestTimeStep();
            // if there is at least one pedestrian inCollision then the smallest
            // variable step is a in collision step.
            if (!this.pedestrianToMoveInCollision.isEmpty())
                this.moveInCollisionStep(currentTime);
            else{
                // check the in range step
                if (!this.pedestrianToMoveInRange.isEmpty())
                    // if at least one is in range and the rest is at isolation or in range
                    // then the smallest simulation step is in range
                    this.moveInRangeStep(currentTime);
                else
                    // else move all pedestrians with the isolation step
                    this.moveInIsolationStep(currentTime);
            }
        }
        else{
            // if it is not a variable time step
            // move the pedestrians in queues
            //movePedestriansInQueues(this.isolatedTimeStepSeconds, currentTime);
            // move all pedestrians according to the simulation step
            walkPedestrians(this.pedestrianToMoveInCollision, this.isolatedTimeStepSeconds, currentTime);
            // if it is a parallel update then update the next position of pedestrians
            //if (Pedestrian.isParallel)
            //    Pedestrian.updateParallel(this.pedestrianToMoveInCollision);
        }

        Pedestrian.updateAccelerations(this.pedestrianToMoveInCollision, this.timeStep/1000.0);
    }

    /**
     * Check if this pedestrian has to be included in the event step array.
     * @param pedestrian
     */
    private void insertPedInMoveList(Pedestrian pedestrian) {

        if (Pedestrian.isVariableStep()){
            if (pedestrian.getIsolationTypePed() == NOMAD.IN_COLLISION ||
                    pedestrian.getIsolationTypeObs() == NOMAD.IN_COLLISION)
                this.pedestrianToMoveInCollision.add(pedestrian);
            else{
                if (pedestrian.getIsolationTypePed() == NOMAD.IN_RANGE ||
                        pedestrian.getIsolationTypeObs() == NOMAD.IN_RANGE)
                    this.pedestrianToMoveInRange.add(pedestrian);
                else
                    this.pedestrianToMoveInIsolation.add(pedestrian);
            }
        }
        else
            // if the time step is constant
            this.pedestrianToMoveInCollision.add(pedestrian);
    }


    private void moveInIsolationStep(int currentTime) {
        // move the pedestrians in queues
        //movePedestriansInQueues(this.isolatedTimeStepSeconds, currentTime);
        // move in isolation
        walkPedestrians(this.pedestrianToMoveInIsolation, this.isolatedTimeStepSeconds, currentTime);
        if (Pedestrian.isParallel)
            Pedestrian.updateParallel(this.pedestrianToMoveInIsolation);
    }

    private void moveInRangeStep(int currentTime) {

        // ask the pedestrians in isolation to move
        walkPedestrians(this.pedestrianToMoveInIsolation, this.isolatedTimeStepSeconds, currentTime);

        // for each in range step
        for (double rangeStep = this.rangeTimeStepMillis;rangeStep <= this.isolatedTimeStepMillis; rangeStep+= this.rangeTimeStepMillis) {
            // move the pedestrians in queues
            //movePedestriansInQueues(this.rangeTimeStepSeconds, currentTime);
            // ask the in range pedestrians to perform the activity(walking included)
            walkPedestrians(this.pedestrianToMoveInRange, this.rangeTimeStepSeconds, currentTime);
            if (Pedestrian.isParallel)
                Pedestrian.updateParallel(this.pedestrianToMoveInRange);
        }
        // after the in range steps finished try to walk the residue
        // of the simulation step
        if (this.remainderInRangeMillis>0){
            // move the pedestrians in queues
            //movePedestriansInQueues(this.remainderInRangeSeconds, currentTime);
            walkPedestrians(this.pedestrianToMoveInRange, this.remainderInRangeSeconds, currentTime);
            if (Pedestrian.isParallel)
                Pedestrian.updateParallel(this.pedestrianToMoveInRange);
        }
        if (Pedestrian.isParallel)
            Pedestrian.updateParallel(this.pedestrianToMoveInIsolation);
    }

    private void moveInCollisionStep(int currentTime) {
        int rangeStep = this.rangeTimeStepMillis;

        // ask the pedestrians in isolation to move but do not update their position
        // in case of parallel walking
        walkPedestrians(this.pedestrianToMoveInIsolation, this.isolatedTimeStepSeconds, currentTime);

        // for each in collision step
        for (int colStep = this.collisionTimeStepMillis;colStep <= this.isolatedTimeStepMillis; colStep+= this.collisionTimeStepMillis) {
            // move the pedestrians in queues
            //movePedestriansInQueues(this.collisionTimeStepSeconds, currentTime);
            // ask the in collision pedestrians to perform the activity(walking included)
            walkPedestrians(this.pedestrianToMoveInCollision, this.collisionTimeStepSeconds, currentTime);
            // check if the range step is reached
            if (rangeStep<= colStep){
                walkPedestrians(this.pedestrianToMoveInRange, this.rangeTimeStepSeconds, currentTime);
                rangeStep += this.rangeTimeStepMillis;
            }

            // if it is a parallel update then update the next position of pedestrians
            if (Pedestrian.isParallel){
                Pedestrian.updateParallel(this.pedestrianToMoveInCollision);
                Pedestrian.updateParallel(this.pedestrianToMoveInRange);
            }
        }
        // after the in collision steps finished try to walk the residue
        // of the simulation step
        if (this.remainderInCollisionMillis>0){
            // move the pedestrians in queues
            //movePedestriansInQueues(this.remainderInCollisionSeconds, currentTime);
            walkPedestrians(this.pedestrianToMoveInCollision, this.remainderInCollisionSeconds, currentTime);
            if (Pedestrian.isParallel)
                Pedestrian.updateParallel(this.pedestrianToMoveInCollision);
            // ask the in range to make the last step before the remainder
            if (rangeStep<= this.isolatedTimeStepMillis){
                walkPedestrians(this.pedestrianToMoveInRange, this.rangeTimeStepSeconds, currentTime);
                if (Pedestrian.isParallel)
                    Pedestrian.updateParallel(this.pedestrianToMoveInRange);
            }
        }

        // after the in collision steps finished try to walk the residue
        // of the simulation step
        if (this.remainderInRangeMillis>0){
            walkPedestrians(this.pedestrianToMoveInRange, this.remainderInRangeSeconds, currentTime);
            if (Pedestrian.isParallel)
                Pedestrian.updateParallel(this.pedestrianToMoveInRange);
        }

        if (Pedestrian.isParallel)
            Pedestrian.updateParallel(this.pedestrianToMoveInIsolation);
    }

    private static void walkPedestrians(ArrayList<Pedestrian> pedestrians, double timeStep, int currentTime){
        for (Pedestrian pedestrian: pedestrians){
            // in  Nomad walking is also considered an activity
/*			if (pedestrian.getPedId()==40)
				System.out.print("stop ped manager");*/

/*			if (pedestrian.getSpeed().length()==0&& pedestrian.getLevel().getName().compareToIgnoreCase("escada2_via2")==0)
	System.out.println("stop nomad level1");*/
            // therefore we step the activities
            pedestrian.walk(timeStep, currentTime);
        }
    }

/*
    private static void movePedestriansInQueues(double timeStep, int currentTime){

        // change this method
        //NomadModel.model.infrastructureXML.getTurnstiles(nextDestination)
        // go through all turnstiles

        for (Server server: NomadModel.model.infrastructureXML.getServers()){
            if (server.isInUse()) {
                server.getQueue1().movePedestrians(timeStep, currentTime);
                if (server.isBidirectional())
                    server.getQueue2().movePedestrians(timeStep, currentTime);
            }
        }
    }*/
}
