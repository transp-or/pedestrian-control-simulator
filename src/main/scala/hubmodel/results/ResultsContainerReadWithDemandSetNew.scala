package hubmodel.results

import hubmodel.io.input.JSONReaders.PedestrianResults_JSON


class ResultsContainerReadWithDemandSetNew(tt: Vector[PedestrianResults_JSON],
                                           monitoredAreaDensity: Option[Map[String, Vector[(tools.Time, Double)]]],
                                           monitoredAreaIndividualDensity: Option[Map[String, Vector[(tools.Time, Vector[Double])]]],
                                           val demandFile: String) extends ResultsContainerReadNew(tt, monitoredAreaDensity, monitoredAreaIndividualDensity, None)