package hubmodel.io.input.JSONReaders

import hubmodel.supply.Infrastructure

/**
  *
  * @param amwsMode    main location
  * @param subLocation subarea
  * @param walls       vector storing the walls
  * @param doors       collection of doors
  */
case class ContinuousSpaceParserWithDoors(amwsMode: String,
                                          setup: String,
                                          walls: Vector[Wall_JSON],
                                          doors: Vector[Doorway_JSON]) extends Infrastructure