package hubmodel.control.amw

import hubmodel.Position
import hubmodel.control.ControlDevicePolicy
import hubmodel.prediction.AMWFlowsFromEmpiricalData
import hubmodel.supply.graph.readGraph
import optimization.ALNS.{DirectionMatchFlow, enforceSpeedChangeIntoPolicy}
import tools.cells.Vertex
import trackingdataanalysis.pedtrack.{ReadTrackingData, ZoneProcessingNew}

class StaticEngineeringSolution(vertices: Map[String, Vertex], amws: Iterable[MovingWalkway]) {

  print(" * computing static engineering solution...")

  private val zoneProcessor = new ZoneProcessingNew("E:\\PhD\\hub-simulator\\piw-corridor\\graph.json")

  private val files = Vector(
    "E:\\PhD\\hub-simulator\\tracking-data-lausanne\\tracking-data-lausanne.tar\\tracking-data-lausanne\\lausanne_2013_01_22_piw.csv",
    "E:\\PhD\\hub-simulator\\tracking-data-lausanne\\tracking-data-lausanne.tar\\tracking-data-lausanne\\lausanne_2013_01_23_piw.csv",
    "E:\\PhD\\hub-simulator\\tracking-data-lausanne\\tracking-data-lausanne.tar\\tracking-data-lausanne\\lausanne_2013_02_06_piw.csv",
    "E:\\PhD\\hub-simulator\\tracking-data-lausanne\\tracking-data-lausanne.tar\\tracking-data-lausanne\\lausanne_2013_02_27_piw.csv"/*,
    "E:\\PhD\\hub-simulator\\tracking-data-lausanne\\tracking-data-lausanne.tar\\tracking-data-lausanne\\lausanne_2013_02_28_piw.csv",
    "E:\\PhD\\hub-simulator\\tracking-data-lausanne\\tracking-data-lausanne.tar\\tracking-data-lausanne\\lausanne_2013_03_05_piw.csv",
    "E:\\PhD\\hub-simulator\\tracking-data-lausanne\\tracking-data-lausanne.tar\\tracking-data-lausanne\\lausanne_2013_04_09_piw.csv",
    "E:\\PhD\\hub-simulator\\tracking-data-lausanne\\tracking-data-lausanne.tar\\tracking-data-lausanne\\lausanne_2013_04_20_piw.csv",
    "E:\\PhD\\hub-simulator\\tracking-data-lausanne\\tracking-data-lausanne.tar\\tracking-data-lausanne\\lausanne_2013_04_18_piw.csv",
    "E:\\PhD\\hub-simulator\\tracking-data-lausanne\\tracking-data-lausanne.tar\\tracking-data-lausanne\\lausanne_2013_04_30_piw.csv"*/)

  private val pop: Vector[hubmodel.ped.Pedestrian] = files.flatMap(f => new ReadTrackingData(f).population.values.map(p => {
    val zonesByPositions: Vector[(tools.Time, Option[Vertex], Position)] = p.getHistoryPosition.map(pos => (pos._1, zoneProcessor.findZone(pos._2.pos), pos._2.pos))
    zonesByPositions
      .filter(_._2.isDefined)
      .sliding(2)
      .filter(s => s(0)._2 != s(1)._2)
      .foreach(v =>  p.appendAccomplishedRoute(v(1)._1, v(1)._2.get, v(1)._3))
    p
  }))

  private val intervals = (25200 to 27000 by 10).toVector.map(t => tools.Time(t))
  private val r = new AMWFlowsFromEmpiricalData(pop, intervals, Map("amw1" -> Vector(Vector("b","c","d").map(zoneProcessor.vertices)), "amw2" -> Vector(Vector("d", "e").map(zoneProcessor.vertices))))

  private val initialControlPolicy = amws
    .flatMap(w => intervals.zip(Vector.fill(intervals.size)((w.name, w.length))).map(t => (t._2, t._1)))
    .map(t => AMWPolicy(t._1._1, t._2, t._2 + tools.Time(10), 0.0, t._1._2)).toVector

  val staticPolicyEngineeringSolution: (Vector[ControlDevicePolicy], Vector[MovingWalkwayControlEvents]) =
    enforceSpeedChangeIntoPolicy(new DirectionMatchFlow(Vector(r.aggregateFlowsByAMW), intervals).xprime(initialControlPolicy), Map("amw1" -> 0.0, "amw2"-> 0.0))

  println(" done !")

}
