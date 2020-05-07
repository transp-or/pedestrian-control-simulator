package hubmodel.results

import hubmodel.io.input.JSONReaders.PedestrianResults_JSON


class ResultsContainerReadWithDemandSetNew(tt: Vector[PedestrianResults_JSON],
                                           monitoredAreaDensity: Option[(Vector[Double], Vector[Vector[Double]])],
                                           monitoredAreaIndividualDensity: Option[Vector[(BigDecimal, BigDecimal)]],
                                           val demandFile: String) extends ResultsContainerReadNew(tt, monitoredAreaDensity, monitoredAreaIndividualDensity, None)