package hubmodel.input.JSONReaders

import hubmodel.supply.Infrastructure

/** For reading JSON files storing the specs
  *
  * @param location    main location
  * @param subLocation subarea
  * @param walls       vector storing the walls
  */
case class ContinuousSpaceParser(location: String,
                                 subLocation: String,
                                 walls: Vector[Wall_JSON]) extends Infrastructure