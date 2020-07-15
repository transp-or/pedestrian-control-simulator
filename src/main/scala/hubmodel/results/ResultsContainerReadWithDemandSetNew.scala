package hubmodel.results

import hubmodel.io.input.JSONReaders.{DensityData_JSON, PedestrianResults_JSON}


class ResultsContainerReadWithDemandSetNew(id: String, tt: Vector[PedestrianResults_JSON],
                                           monitoredAreaDensity: Option[Map[String, DensityData_JSON]],
                                           //monitoredAreaIndividualDensity: Option[Map[(String, String), Vector[(tools.Time, Vector[Double])]]],
                                           val demandFile: String) extends ResultsContainerReadNew(id, tt, monitoredAreaDensity, None)