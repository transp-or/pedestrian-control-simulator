package hubmodel.input.JSONReaders

import hubmodel.supply.Infrastructure

/**
  *
  * @param location    main location
  * @param subLocation subarea
  * @param walls       vector storing the walls
  * @param doors       collection of doors
  */
case class ContinuousSpaceParserWithDoors(location: String,
                                          subLocation: String,
                                          walls: Vector[Wall_JSON],
                                          doors: Vector[Doorway_JSON]) extends Infrastructure