package hubmodel.results


class ResultsContainerReadWithDemandSet(tt: Vector[(String, String, Double, Double, Double)],
                                        monitoredAreaDensity: Option[(Vector[Double], Vector[Vector[Double]])],
                                        monitoredAreaIndividualDensity: Option[Vector[(BigDecimal, BigDecimal)]], val demandFile: String) extends ResultsContainerRead(tt, monitoredAreaDensity, monitoredAreaIndividualDensity)