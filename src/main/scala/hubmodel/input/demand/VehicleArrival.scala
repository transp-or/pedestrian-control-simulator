package hubmodel.input.demand

/**
  * Created by nicholas on 5/12/17.
  */

import hubmodel.input.infrastructure.{NodeID, ODID, TrainID}
import hubmodel.{Action, SFGraphSimulator}

/** Insert the arrivals of all vehicle in the event list. The trains variables stored in the simulation is the
  * variable which is used.
  *
  * @param sim simulator containing the data
  */
class InsertVehicleArrivals(sim: SFGraphSimulator) extends Action {

  /**
    * Execution of the event.
    */
  override def execute(): Unit = {
    sim.eventLogger.trace("time=" + sim.currentTime + ": inserting vehicles")
    sim.timeTable.trains.filter(t => t._2.arr.isDefined).foreach(t => sim.insertEventAtAbsolute(t._2.arr.get.toSecondOfDay){new VehicleArrival(t._2, sim) } )
  }
}

/** Models the arrival of a train at the station. This is the class which stores the train induced flow (TINF)
  * funcionality.
  *
  * TODO: This should be made more generic, with a better implementation. Look at Flurin's code for inspiration.
  * Need to adapt it to more generic situations then trains (metros, buses, trams).
  *
  * @param train Train instance to insert
  * @param sim simulation environment
  */
class VehicleArrival(train: Train, sim: SFGraphSimulator) extends Action {
  override def execute(): Unit = {
    sim.eventLogger.trace("time=" + sim.currentTime + ": vehicle arrival")
    trainInducedFlow(train).foreach(flow => sim.insertEventWithDelay(0){new PedestrianGeneration(flow.O, flow.D, flow.start.toSecondOfDay, flow.end.toSecondOfDay, flow.f, sim)})


    // start and end must be relative to train arrival !!!!
    def trainInducedFlow(t: Train): Vector[PedestrianFlow] = {
      def connectionsToSplitFractionsHelper(arrNodes: Iterable[NodeID], depNodes: Iterable[NodeID]): Vector[(NodeID, NodeID, Double)] = {
        val perm = for {// all permutations of two lists of nodes
          a <- arrNodes
          b <- depNodes
          if b != a
        } yield {
          (a, b)
        }
        perm.map(p => (p._1, p._2, 1.0 / perm.size)).toVector // split fractions over all permutations. Not realistic but a start
      }

      val departingFlows = sim.pedestrianFlows.PTflows.filter(f => f.origin == t.ID)

      /** Vector of required connections extracted from the passenger flows */
      val requiredLinks: Vector[(ODID, ODID)] = departingFlows.map(p => (p.origin, p.destination))

      /** Union of both mapping functions, trains to NodeID and strings to NodeID*/
      val OD2Nodes: String => Vector[NodeID] = str => {
        sim.timeTable.train2Nodes(str) match {
          case Some(x) => x
          case None => sim.nodeNaming.string2Int(str) match {
            case Some(x) => x
            case None => throw new Exception("Name mappings are missing ! Either wrong train number of wrong OD names.")
          }
        }
      }

      /** Map from connecting trains to corresponding nodes */
      val linksAsNodesMap: Map[(ODID, ODID), (Iterable[NodeID], Iterable[NodeID])] = {
        requiredLinks.map(c => c -> (OD2Nodes(c._1), OD2Nodes(c._2))).toMap
      }

      /** Train connections to split fractions map */
      val connectionsToSplitFractions: Map[(ODID, ODID), Vector[(NodeID, NodeID, Double)]] =
        linksAsNodesMap.map(c => c._1 -> connectionsToSplitFractionsHelper(c._2._1, c._2._2))

      departingFlows
        .filter(PTFlow => {!sim.timeTable.isOnSamePlatform(PTFlow.origin, PTFlow.destination)})
        .flatMap(p => {
          connectionsToSplitFractions(p.origin, p.destination).map(c =>
            PedestrianFlow(c._1, c._2, string2LocalTime("00:00:00"), string2LocalTime("00:00:00").plusSeconds(120), math.round(p.f * c._3))
          ) // very strong assumption: alighting happens in 2 minutes. Need to relax this later on
        })
    }
    }

    //sim.insertEvent(1)( new PedestrianGeneration(sim.currentTime, sim.currentTime + 15, 15) )

}
