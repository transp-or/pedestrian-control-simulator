package hubmodel.io.input.JSONReaders

import hubmodel.supply.Infrastructure

/** For reading JSON files storing the specs
  *
  * @param amwsMode    main location
  * @param subLocation subarea
  * @param walls       vector storing the walls
  */
case class ContinuousSpaceParser(amwsMode: String,
                                 subLocation: String,
                                 walls: Vector[Wall_JSON]) extends Infrastructure