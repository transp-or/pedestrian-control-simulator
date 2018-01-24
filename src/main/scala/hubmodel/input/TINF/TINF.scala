package hubmodel.input.TINF

/*
class Door(val width: Double) {
  val flowrate: Double = width*0.7
}
class PTVehicle(val length: Double, val capacity: Double) {
  def createDoors
  val doors: Seq[(Double, Door)] = Vector((2, new Door(2.1, 3)), (4, new Door(4.2,6)))
}

/* ----------------------------------------------------------------------------------
                            TRAIN INDUCED FLOWS (TINF)
-----------------------------------------------------------------------------------*/
// assuming uniform access distribution
val splitFractions: Iterable[NodeID] => Vector[Double] = nodes => Vector.fill(nodes.size){1.0/nodes.size}

// assumes passenger use infrastructure to capacity


def timeTable2Inflow(infra: InfraOD, trainSchedule: TrainTimeTable, trainPassengers: Vector[PassengerFlow]): Vector[Flow] = {
def connectionsToSplitFractionsHelper(arrNodes: Iterable[NodeID], depNodes: Iterable[NodeID]): Vector[(NodeID, NodeID, Double)] = {
val perm = for { // all permutations of two lists of nodes
a <- arrNodes
b <- depNodes
if b != a
} yield {(a,b)}
perm.map(p => (p._1, p._2, 1.0/perm.size)).toVector // split fractions over all permutations. Not realistic but a start
}

/** Vector of required connections extracted from the passenger flows */
val requiredConnections: Vector[(String, String)] = trainPassengers.map(p => (p.O, p.D))
/** Map from connecting trains to corresponding nodes */
val connectionsToNodes: Map[(String, String),(Iterable[NodeID], Iterable[NodeID])] = requiredConnections.map(c => c -> (infra.platform2Node(infra.track2platform(trainSchedule.train2Track(c._1))), infra.platform2Node(infra.track2platform(trainSchedule.train2Track(c._2))))).toMap

/** Train connections to split fractions map */
val connectionsToSplitFractions: Map[(String, String),Vector[(NodeID, NodeID, Double)]] = connectionsToNodes.map(c => c._1 -> connectionsToSplitFractionsHelper(c._2._1, c._2._2))

trainPassengers
.filter(PTFlow => !infra.isOnSamePlatform(trainSchedule.train2Track(PTFlow.O), trainSchedule.train2Track(PTFlow.D)))
.flatMap(p => {
connectionsToSplitFractions(p.O, p.D).map(c =>
Flow(trainSchedule.timeTable(p.O).arr, trainSchedule.timeTable(p.O).arr.plusSeconds(180), c._1, c._2, p.flow*c._3)
) // very strong assumption: alighting happens in 3 minutes. Need to relax this later on
}
)
}*/