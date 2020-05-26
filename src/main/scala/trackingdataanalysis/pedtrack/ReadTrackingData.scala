package trackingdataanalysis.pedtrack

import hubmodel.Position
import trackingdataanalysis.TrackingDataFormat

class ReadTrackingData(file: String) extends TrajectoryProcessing {

    // try and guess the data format from the file
    val dataType: TrackingDataFormat = guessDataFormat(file)

    // read the data file and build the population
    val population: Map[Int, hubmodel.ped.Pedestrian] = buildPedestrianTrajectories(dataType)
}
