package hubmodel.HubOutput


/**
  * Created by nicholas on 3/22/17.
  */
package object ResultsProcessing {

  /*
  type PedestrianMap = Vector[PedestrianSim]


  // implicit class which works on the PedestrianMap Type.
  implicit class MapPedestrianFunctions(x: PedestrianMap) extends FunctionsOnPedestrianMaps {

    // Filter by time. The start and end parameters have default values set to 0 and 24h (86400s)
    def filterByTime(start: Double = 0, end: Double = 86400): PedestrianMap = {
      x.filter(p => p.entryTime > start && p.exitTime < end)
    }

    // Filter by OD. This method is overloaded with variants taking lists of OD pairs.
    def filterByOD(O: Int, D: Int): PedestrianMap = {
      x.filter(p => p.oZone == O && p.dZone == D)
    }

    def filterByOD(O: List[Int], D: List[Int]): PedestrianMap = {
      x.filter(p => O.contains(p.oZone) && D.contains(p.dZone))
    }

    def filterByOD(O: Int, D: List[Int]): PedestrianMap = {
      x.filter(p => p.oZone == O && D.contains(p.dZone))
    }

    def filterByOD(O: List[Int], D: Int): PedestrianMap = {
      x.filter(p => O.contains(p.oZone) && p.dZone == D)
    }

    def filterByOrigin(O: Int): PedestrianMap = {
      x.filter(p => p.oZone == O)
    }

    def filterByOrigin(O: List[Int]): PedestrianMap = {
      x.filter(p => O.contains(p.oZone))
    }

    def filterByDestination(D: Int): PedestrianMap = {
      x.filter(p => p.dZone == D)
    }

    def filterByDestination(D: List[Int]): PedestrianMap = {
      x.filter(p => D.contains(p.dZone))
    }

    // Input is list of OD pairs
    def filterByODPairs(OD: List[(Int, Int)]): PedestrianMap = {
      x.filter(p => OD.contains((p.oZone, p.dZone)))
    }

    // extract specific data from pedestrians to create histograms
    def mapQuantityToVector(data: String): scala.Vector[Double] = {
      data match {
        case "travelDistance" => x.map(p => p.travelDistance).toVector
        case "travelTime" => x.map(p => p.travelTime).toVector
        case "meanVelocity" => x.map(p => p.freeFlowVel).toVector
        case other => throw new IllegalArgumentException("choice is not valid")
      }
    }

    def writeQuantityToCSV(data: String, filename: String, path: String = ""): Unit = {
      x.mapQuantityToVector(data).writeToCSV(filename, path)
    }

    def writeQuantityToJSON(data: String, filename: String, path: String = ""): Unit = {
      x.mapQuantityToVector(data).writeToJSON(data, filename, path)
    }
  }

  // implicit class for multi day results
  implicit class ListMapPedestrianFunctions(x: Map[DataSpecification, PedestrianMap]) extends FunctionsOnPedestrianMaps {
    // Filter by time. The start and end parameters have default values set to 0 and 24h (86400s)
    def filterByTime(start: Double = 0, end: Double = 86400): Map[DataSpecification, PedestrianMap] = {
      x.mapValues(pedMap => pedMap.filter(p => p.entryTime > start && p.exitTime < end))
    }

    // Filter by OD. This method is overloaded with variants taking lists of OD pairs.
    def filterByOD(O: Int, D: Int): Map[DataSpecification, PedestrianMap] = {
      x.mapValues(pedMap => pedMap.filter(p => p.oZone == O && p.dZone == D))
    }

    def filterByOD(O: List[Int], D: List[Int]): Map[DataSpecification, PedestrianMap] = {
      x.mapValues(pedMap => pedMap.filter(p => O.contains(p.oZone) && D.contains(p.dZone)))
    }

    def filterByOD(O: Int, D: List[Int]): Map[DataSpecification, PedestrianMap] = {
      x.mapValues(pedMap => pedMap.filter(p => p.oZone == O && D.contains(p.dZone)))
    }

    def filterByOD(O: List[Int], D: Int): Map[DataSpecification, PedestrianMap] = {
      x.mapValues(pedMap => pedMap.filter(p => O.contains(p.oZone) && p.dZone == D))
    }

    def filterByOrigin(O: Int): Map[DataSpecification, PedestrianMap] = {
      x.mapValues(pedMap => pedMap.filter(p => p.oZone == O))
    }

    def filterByOrigin(O: List[Int]): Map[DataSpecification, PedestrianMap] = {
      x.mapValues(pedMap => pedMap.filter(p => O.contains(p.oZone)))
    }

    def filterByDestination(D: Int): Map[DataSpecification, PedestrianMap] = {
      x.mapValues(pedMap => pedMap.filter(p => p.dZone == D))
    }

    def filterByDestination(D: List[Int]): Map[DataSpecification, PedestrianMap] = {
      x.mapValues(pedMap => pedMap.filter(p => D.contains(p.dZone)))
    }

    // Input is list of OD pairs
    def filterByODPairs(OD: List[(Int, Int)]): Map[DataSpecification, PedestrianMap] = {
      x.mapValues(pedMap => pedMap.filter(p => OD.contains((p.oZone, p.dZone))))
    }

    // extract specific data from pedestrians to create histograms and merge into same vector
    def mapQuantityToVector(data: String): scala.Vector[Double] = {
      data match {
        case "travelDistance" => x.flatMap(pedMap => pedMap._2.map(p => p.travelDistance)).toVector
        case "travelTime" => x.flatMap(pedMap => pedMap._2.map(p => p.travelTime)).toVector
        case "meanVelocity" => x.flatMap(pedMap => pedMap._2.map(p => p.freeFlowVel)).toVector
        case other => throw new IllegalArgumentException("choice is not valid")
      }
    }

    // keep day separatation
    def mapToVectorByDay(data: String): Map[DataSpecification, Vector[Double]] = {
      data match {
        case "travelDistance" => x.mapValues(pedMap => pedMap.map(p => p.travelDistance))
        case "travelTime" => x.mapValues(pedMap => pedMap.map(p => p.travelTime))
        case "meanVelocity" => x.mapValues(pedMap => pedMap.map(p => p.freeFlowVel))
        case other => throw new IllegalArgumentException("choice is not valid")
      }
    }

    // Sort data and write to CSV
    def writeQuantityToCSV(data: String, filename: String, path: String = ""): Unit = {
      x.mapToVectorByDay(data).writeToCSV(filename, path)
    }

    // Sort by data and write to JSON
    def writeQuantityToJSON(data: String, filename: String, path: String = ""): Unit = {
      x.mapToVectorByDay(data).writeToJSON(data, filename, path)
    }
  }
*/
}
