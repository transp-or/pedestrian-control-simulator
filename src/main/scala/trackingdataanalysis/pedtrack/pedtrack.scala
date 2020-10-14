package trackingdataanalysis.pedtrack


/**
  * Gives the list of filters which should be implemented
  */
trait FunctionsOnPedestrianMaps {

  /** Returns pedestrian which entered after start and exited before end
    *
    * @param start entry time limit
    * @param end   exit time limit
    * @return the filtered list of pedestrians. Type is unspecified as it will depend on the input type
    */
  def filterByTime(start: Long, end: Long): Any

  /** Returns pedestrians which entered and exited at specific nodes
    *
    * @param O origin node
    * @param D destination node
    * @return the filtered list of pedestrians. Type is unspecified as it will depend on the input type
    */
  def filterByOD(O: Int, D: Int): Any

  /** Returns pedestrians which entered or exited in given lists of nodes
    *
    * @param O List of origin nodes
    * @param D List of exit nodes
    * @return the filtered list of pedestrians. Type is unspecified as it will depend on the input type
    */
  def filterByOD(O: List[Int], D: List[Int]): Any

  /** Returns the pedestrians which entered in O and exit in a list of exit nodes
    *
    * @param O specific entrance node
    * @param D list of exit nodes
    * @return the filtered list of pedestrians. Type is unspecified as it will depend on the input type
    */
  def filterByOD(O: Int, D: List[Int]): Any

  /** Returns the pedestrians which entered via the list O and exited at D
    *
    * @param O list of entrance nodes
    * @param D specific exit node
    * @return the filtered list of pedestrians. Type is unspecified as it will depend on the input type
    */
  def filterByOD(O: List[Int], D: Int): Any

  /** Returns the pedestrians entering via a single node
    *
    * @param O entrance node
    * @return the filtered list of pedestrians. Type is unspecified as it will depend on the input type
    */
  def filterByOrigin(O: Int): Any

  /** Returns the pedetrians who entered in a list of node
    *
    * @param O list of entrance nodes
    * @return the filtered list of pedestrians. Type is unspecified as it will depend on the input type
    */
  def filterByOrigin(O: List[Int]): Any

  /** Returns the pedestrians who exited through a node
    *
    * @param D unique exit node
    * @return the filtered list of pedestrians. Type is unspecified as it will depend on the input type
    */
  def filterByDestination(D: Int): Any

  /** Returns the pedestrians who exited via a list of nodes
    *
    * @param D list of exit nodes
    * @return the filtered list of pedestrians. Type is unspecified as it will depend on the input type
    */
  def filterByDestination(D: List[Int]): Any

  /** Returns pedestrians who accomplished specfiic OD pairs
    *
    * @param OD list of OD tuples
    * @return the filtered list of pedestrians. Type is unspecified as it will depend on the input type
    */
  def filterByODPairs(OD: List[(Int, Int)]): Any

  /** Returns a vector containing the values of some member of PedestrainTrait for all pedestrians.
    *
    * @param data String which matches exactly one member of PedestrianTrait
    * @return vector containing the data
    *
    */
  def mapQuantityToVector(data: String): Vector[Double]

}

/** Identifier based on the geographical location, day and sublocation to use as keys in maps.
  * The information is extracted based on matching the locations "lausanne" or "basel" and then counting
  * characters. Not robust to different file naming scheme.
  *
  * @param _file string containing the filename to analyse
  */
case class DataSpecification(private val _file: String) {
  private val city: String = {
    if (_file.toLowerCase.lastIndexOf("lausanne") >= 0) "lausanne"
    else if (_file.toLowerCase.lastIndexOf("basel") >= 0) "basel"
    else "missing"
  }

  private val cityStart: Int = _file.lastIndexOf(city)
  private val date: Int = {
    if (city == "missing") 0
    else _file.slice(cityStart + city.length + 1, cityStart + city.length + 11).replaceAll("[^\\p{L}\\p{Nd}]+", "").toInt
  }

  private val extensionStart: Int = _file.lastIndexOf(".csv")
  private val location: String = {
    if (city == "missing") "NA"
    else _file.slice(cityStart + city.length + 1 + 11, extensionStart)
  }

  val fileName: String = _file.drop(_file.lastIndexOf("/") + 1).dropRight(4)

  override def toString: String = this.city + "-" + this.date.toString + "-" + this.location
}

/** Time identifier
  *
  * @param Y  year
  * @param M  month
  * @param D  day
  * @param h  hours
  * @param m  minute
  * @param s  second
  * @param ms millisecond, default = 0
  */
case class Time(Y: Int, M: Int, D: Int, h: Int, m: Int, s: Int, ms: Int = 0) {

  /** Number of seconds from 00:00 of the day */
  val toSecondsOfDay: Double = h * 3600.0 + m * 60.0 + s + ms.toDouble / 1000.0

  /** Builds the object from an array of length 7
    *
    * @param arr arr containing the timestamp
    */
  def this(arr: Array[Int]) {
    this(arr(0), arr(1), arr(2), arr(3), arr(4), arr(5), arr(6))
  }

  /** Creates a string from the Time object
    *
    * @return formated as a String
    */
  override def toString: String = "%04d-%02d-%02d %02d:%02d:%02d:%03d".format(Y, M, D, h, m, s, ms)

} // end of Time

