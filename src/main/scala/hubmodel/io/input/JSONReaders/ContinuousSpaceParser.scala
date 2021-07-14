package hubmodel.io.input.JSONReaders

import hubmodel.supply.Infrastructure

/** For reading JSON files storing the specs
  *
  * @param amwsMode    main location
  * @param setup subarea
  * @param walls       vector storing the walls
  */
case class ContinuousSpaceParser(amwsMode: String,
                                 setup: String,
                                 walls: Vector[Wall_JSON]) extends Infrastructure