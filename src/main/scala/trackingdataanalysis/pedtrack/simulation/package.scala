package trackingdataanalysis.pedtrack

/**
  * Created by nicholas on 3/22/17.
  */
package object simulation {

  // typedef for the map of pedestrians. The key is the pedestrian ID
  type PedestrianMap = collection.immutable.Map[Int, PedestrianSim]

  // typedef for for the processing of multiple days simultaneously
  type MultidayPedestrianMap = collection.immutable.Map[DataSpecification, PedestrianMap]


}
