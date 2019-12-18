package demandestimation.network

import hubmodel.Position
import hubmodel.supply.graph.MyRawEdge
import tools.cells.{Point, Vertex}
import tools.graph.Graph

import scala.collection.immutable.ListMap


class NetworkLausanne {

  val positions: ListMap[String, Vertex] = ListMap[String, Vertex]() ++ Vector(
    new Point("NW", new Position(-0.7, 20)),
    new Point("NWM", new Position(1, 20)),
    new Point("NE", new Position(7, 20)),
    new Point("NEM", new Position(9, 20)),
    new Point("SW", new Position(0, -3)),
    new Point("SE", new Position(8, -3)),
    new Point("N", new Position(3, 20)),
    new Point("1D", new Position(-2.5, 14)),
    new Point("1C", new Position(3, 14)),
    new Point("BAR", new Position(-0.8, 16.1)),
    new Point("nww", new Position(-1.4, 18)),
    new Point("KIOSK", new Position(0.9, 12)),
    new Point("kioskh", new Position(0, 12)),
    new Point("SHOP", new Position(-1.5, -3)),
    new Point("1h1", new Position(-2, 14)),
    new Point("1h2", new Position(-1.7, 14)),
    new Point("1d", new Position(-0.9, 14)),
    new Point("nwh", new Position(0, 17)),
    new Point("1wh", new Position(0, 16)),
    new Point("1w", new Position(0, 14)),
    new Point("1wc", new Position(0.9, 14)),
    new Point("1c", new Position(1.4, 14)),
    new Point("1h3", new Position(1.8, 14)),
    new Point("1h4", new Position(2.4, 14)),
    new Point("nh", new Position(3, 19)),
    new Point("h", new Position(3, 18)),
    new Point("34B", new Position(5.5, 9)),
    new Point("34C", new Position(3, 9)),
    new Point("34A", new Position(10.5, 9)),
    new Point("34D", new Position(-2.5, 9)),
    new Point("56B", new Position(5.5, 6)),
    new Point("56C", new Position(3, 6)),
    new Point("56D", new Position(-2.5, 6)),
    new Point("56A", new Position(10.5, 6)),
    new Point("78B", new Position(5.5, 3)),
    new Point("78C", new Position(3, 3)),
    new Point("78A", new Position(10.5, 3)),
    new Point("78D", new Position(-2.5, 3)),
    new Point("9C", new Position(3, 0)),
    new Point("9D", new Position(-2.5, 0)),
    new Point("nw", new Position(-0.7, 18)),
    new Point("sw", new Position(0, -1.5)),
    new Point("nwm", new Position(1, 18)),
    new Point("ne", new Position(7, 16)),
    new Point("neh1", new Position(7, 19)),
    new Point("neh2", new Position(7, 18)),
    new Point("nem", new Position(9, 16)),
    new Point("se", new Position(8, -1.5)),
    new Point("1eh", new Position(8, 15)),
    new Point("1e", new Position(8, 14)),
    new Point("1AB", new Position(10.5, 14)),
    new Point("70FE", new Position(10.5, 16)),
    new Point("70fe", new Position(9.5, 16)),
    new Point("1ab", new Position(9.5, 14)),
    new Point("34d", new Position(-0.9, 9)),
    new Point("34c", new Position(0.9, 9)),
    new Point("34b", new Position(7.1, 9)),
    new Point("34a", new Position(8.9, 9)),
    new Point("56d", new Position(-0.9, 6)),
    new Point("56c", new Position(0.9, 6)),
    new Point("56b", new Position(7.1, 6)),
    new Point("56a", new Position(8.9, 6)),
    new Point("78d", new Position(-0.9, 3)),
    new Point("78c", new Position(0.9, 3)),
    new Point("78b", new Position(7.1, 3)),
    new Point("78a", new Position(8.9, 3)),
    new Point("9d", new Position(-0.9, 0)),
    new Point("9c", new Position(0.9, 0)),
    new Point("9h1", new Position(-0.6, 0)),
    new Point("9h2", new Position(-0.3, 0)),
    new Point("9h3", new Position(0.3, 0)),
    new Point("9h4", new Position(0, 1)),
    new Point("34w", new Position(0, 9)),
    new Point("56w", new Position(0, 6)),
    new Point("78w", new Position(0, 3)),
    new Point("9w", new Position(0, 0)),
    new Point("34e", new Position(8, 9)),
    new Point("56e", new Position(8, 6)),
    new Point("56h", new Position(8, 4)),
    new Point("78e", new Position(8, 3))
  ).map(p => p.name -> p)

  val node_labels: Vector[String] = Vector("NW", "NWM", "NE", "NEM", "SW", "SE", "N", "1D", "1C", "BAR", "nww", "KIOSK", "kiokh", "SHOP", "1h1", "1h2", "1d", "nwh", "1wh", "1w", "1wc", "1c", "1h3", "1h4", "nh", "h", "34B", "34C", "34A", "34D", "56B", "56C", "56D", "56A", "78B", "78C", "78A", "78D", "9C", "9D", "nw", "sw", "nwm", "ne", "neh1", "neh2", "nem", "se", "1eh", "1e", "1AB", "70FE", "70fe", "1ab", "34d", "34c", "34b", "34a", "56d", "56c", "56b", "56a", "78d", "78c", "78b", "78a", "9d", "9c", "9h1", "9h2", "9h3", "9h4", "34w", "56w", "78w", "9w", "34e", "56e", "56h", "78e")
  val centroids_labels: Vector[String] = Vector("NW", "NWM", "NE", "NEM", "SW", "SE", "N", "1D", "1C", "BAR", "KIOSK", "SHOP", "34B", "34C", "34A", "34D", "56B", "56C", "56D", "56A", "78B", "78C", "78A", "78D", "9C", "9D", "1AB", "70FE")
  val not_centroids_labels: Vector[String] = Vector("nww", "kioskh", "1h1", "1h2", "1d", "nwh", "1wh", "1w", "1wc", "1c", "1h3", "1h4", "nh", "h", "nw", "sw", "nwm", "ne", "neh1", "neh2", "nem", "se", "1eh", "1e", "70fe", "1ab", "34d", "34c", "34b", "34a", "56d", "56c", "56b", "56a", "78d", "78c", "78b", "78a", "9d", "9c", "9h1", "9h2", "9h3", "9h4", "34w", "56w", "78w", "9w", "34e", "56e", "56h", "78e")


  val centroids: Vector[String] = Vector("NW", "NWM", "NE", "NEM", "SW", "SE", "N", "KIOSK", "BAR", "SHOP", "1D", "1C", "70FE", "1AB", "34D", "34C", "34B", "34A", "56D", "56C", "56B", "56A", "78D", "78C", "78B", "78A", "9D", "9C")
  val Centroids_Platform: Vector[String] = Vector("1D", "1C", "70FE", "1AB", "34D", "34C", "34B", "34A", "56D", "56C", "56B", "56A", "78D", "78C", "78B", "78A", "9D", "9C")
  val Centroids_No_Platform: Vector[String] = Vector("NW", "NWM", "NE", "NEM", "SW", "SE", "N", "KIOSK", "BAR", "SHOP")
  val Centroids_Entrance_Exit: Vector[String] = Vector("NW", "NWM", "NE", "NEM", "SW", "SE", "N")
  val Centroids_Shop: Vector[String] = Vector("KIOSK", "BAR", "SHOP")

  val platforms: Vector[String] = Vector("1", "3/4", "5/6", "7/8", "9", "70")
  val platformsMap: Map[Int, String] = Map(1 -> "1", 3 -> "34", 4-> "34", 5 -> "56", 6 -> "56", 7-> "78", 8 -> "78", 9 -> "9", 70 -> "70")

  val Centroid_Types_Detail: Vector[String] = Vector("platform", "entrance", "shop")
  val Centroid_Types_RCh: Vector[String] = Vector("platform", "non-platform")

  val ASE_measurement_nodes: Vector[String] = Vector("nw", "nww", "nwm", "1c", "nh", "neh1", "ne", "nem", "sw", "9h2", "9h3", "se")
  val VS_measurement_nodes: Vector[String] = Vector("1h1", "1h4", "70fe", "1ab", "34d", "34c", "34b", "34a", "56d", "56c", "56b", "56a", "78d", "78c", "78b", "78a", "9d", "9c") // TINF nodes
  val other_nodes: Vector[String] = Vector("h", "1wc", "kioskh", "1h2", "1d", "nwh", "1wh", "1w", "1h3", "neh2", "1eh", "1e", "9h1", "9h4", "34w", "56w", "78w", "9w", "34e", "56e", "56h", "78e")
  val VS_west_nodes: Vector[String] = Vector("1wh", "1d", "1wc", "34d", "34c", "56d", "56c", "78d", "78c", "9h4")
  val VS_east_nodes: Vector[String] = Vector("1e", "34b", "34a", "56b", "56a", "56h")
  val VS_nodes: Vector[String] = VS_west_nodes ++ VS_east_nodes


  val edgesSym: Vector[((String, String), Double)] = Vector[((String, String), Double)]() ++:
    // symmetric edges
    Vector(("34d", "34w"), ("56d", "56w"), ("78d", "78w"), ("34w", "34c"), ("56w", "56c"), ("78w", "78c"), ("kioskh", "KIOSK"), ("1w", "1d")).map(e => (e, 3.0)) ++:
    Vector(("34b", "34e"), ("56b", "56e"), ("78b", "78e"), ("34e", "34a"), ("56e", "56a"), ("78e", "78a")).map(e => (e, 5.0)) ++:
    Vector(("34b", "34e"), ("56b", "56e"), ("78b", "78e"), ("34e", "34a"), ("56e", "56a"), ("78e", "78a")).map(e => (e, 5.0)) ++:
    Vector(("34w", "56w"), ("34e", "56e")).map(e => (e, 15.5)) ++:
    Vector(("56w", "78w")).map(e => (e, 14.4)) ++:
    Vector(("56e", "56h"), ("56h", "78e"), ("neh2", "neh1")).map(e => (e, 7.2)) ++:
    Vector(("34w", "kioskh"), ("kioskh", "1w")).map(e => (e, 11.0)) ++:
    Vector(("1h2", "1h1"), ("1h1", "1D"), ("1c", "1h3"), ("1h3", "1h4"), ("1h4", "1C"), ("h", "nh"), ("neh1", "NE")).map(e => (e, 2.0)) ++: // Check length maybe
    Vector(("1w", "BAR"), ("nh", "N")).map(e => (e, 8.0)) ++:
    Vector(("1h3", "h")).map(e => (e, 30.0)) ++:
    Vector(("h", "neh2")).map(e => (e, 75.0)) ++:
    Vector(("78C", "78B")).map(e => (e, 82.0)) ++:
    Vector(("neh2", "70fe")).map(e => (e, 75.0)) ++:
    Vector(("70fe", "70FE")).map(e => (e, 10.0)) ++:
    Vector(("1ab", "1AB")).map(e => (e, 10.0)) ++:
    Vector(("neh2", "1ab")).map(e => (e, 40.0)) ++:
    Vector(("nww", "NW")).map(e => (e, 60.0)) ++:
    Vector(("nww", "1h2")).map(e => (e, 10.0)) ++:
    Vector(("34e", "1e"), ("nem", "NEM")).map(e => (e, 22.0)) ++:
    Vector(("SW", "sw"), ("sw", "9w")).map(e => (e, 5.0)) ++:
    Vector(("9w", "9h2"), ("9h2", "9h1"), ("9h1", "9d")).map(e => (e, 3.5)) ++:
    Vector(("9w", "9h3"), ("9h3", "9c")).map(e => (e, 7.0)) ++:
    Vector(("9w", "9h4"), ("9h4", "78w")).map(e => (e, 8.6)) ++:
    Vector(("9h1", "SHOP"), ("nwm", "NWM")).map(e => (e, 6.0)) ++:
    Vector(("SE", "se"), ("se", "78e")).map(e => (e, 5.5)) ++:
    Vector(("1w", "1wc")).map(e => (e, 3.0)) ++:
    Vector(("nw", "nwh"), ("nwm", "nwh"), ("nwh", "1wh"), ("ne", "1eh"), ("nem", "1eh"), ("1eh", "1e")).map(e => (e, 1.0)) ++:
    Vector(("1wh", "1w")).map(e => (e, 17.5))
  //

  val edgesAsym: Vector[((String, String), Double)] = Vector[((String, String), Double)]() ++: Vector(("34d", "34D"), ("56d", "56D"), ("78d", "78D"), ("9d", "9D"), ("34b", "34B"), ("56b", "56B"), ("78b", "78B"), ("56a", "56A"), ("1d", "1h2"), ("ne", "neh2"), ("nw", "NW")).map(e => (e, 23.06)) ++: // Stairs up   // Edges with asymmetric lengths (stairs, ramps, etc)
    Vector(("34D", "34d"), ("56D", "56d"), ("78D", "78d"), ("9D", "9d"), ("34B", "34b"), ("56B", "56b"), ("78B", "78b"), ("56A", "56a"), ("1h2", "1d"), ("neh2", "ne"), ("NW", "nw")).map(e => (e, 21.51)) ++: // Stairs down
    Vector(("34a", "34A"), ("78a", "78A")).map(e => (e, 25.46)) ++: // Stairs up plus some corridor
    Vector(("34A", "34a"), ("78A", "78a")).map(e => (e, 23.91)) ++: // Stairs down plus some corridor
    Vector(("34c", "34C"), ("56c", "56C"), ("78c", "78C"), ("9c", "9C")).map(e => (e, 40.05)) ++: // Ramps up
    Vector(("34C", "34c"), ("56C", "56c"), ("78C", "78c"), ("9C", "9c")).map(e => (e, 32.97)) ++: // Ramps down
    Vector(("1wc", "1c")).map(e => (e, 33.8)) ++: Vector(("1c", "1wc")).map(e => (e, 25.84)) // Ramp up
  // Ramp down


  val edges_ASE: Vector[(Vertex, Vertex)] = Vector(
    (positions("nw"),  positions("nwh")), // nwIn
    (positions("nwm"), positions("nwh")), // nwmIn
    (positions("nh"),  positions("h")), // nhIn
    (positions("neh1"),positions("neh2")), // neh1In
    (positions("ne"),  positions("1eh")), // neIn
    (positions("nem"), positions("1eh")), // nemIn
    (positions("sw"),  positions("9w")), // swIn
    (positions("9h2"), positions("9w")), // gh2In
    (positions("9h3"), positions("9w")), // gh3In
    (positions("se"),  positions("78e")), // seIn
    (positions("nw"),  positions("NW")), // nwOut
    (positions("nwm"), positions("NWM")), // nwmOut
    (positions("nh"),  positions("N")), // nhOut
    (positions("neh1"),positions("NE")), // neh1Out
    (positions("ne"),  positions("neh2")), // neOut
    (positions("nem"), positions("NEM")), // nemOut
    (positions("sw"),  positions("SW")), // swOut
    (positions("9h2"), positions("9h1")), // g9h2Out
    (positions("9h3"), positions("9c")), // 9h3Out
    (positions("se"),  positions("SE")) // seOut
    //("nww", "NW") //nwwOut MALFUNCTIONING
    //("1c", "1wc") ,  // 1cIn MALFUNCTIONING
    //("1c", "1h3"), //1cOut MALFUNCTIONING
    //("nww", "1h2") , //nwwIn MALFUNCTIONING

  )


  //We create a list of the edges associated to TINF
  val edges_TINF: Vector[(String, String)] = Vector(
    ("1D", "1h1"),
    ("34d", "34w"),
    ("56d", "56w"),
    ("78d", "78w"),
    ("9d", "9h1"),
    ("1C", "1h4"),
    ("34c", "34w"),
    ("56c", "56w"),
    ("78c", "78w"),
    ("9c", "9h3"),
    ("1AB", "1ab"),
    ("70FE", "70fe"),
    ("34b", "34e"),
    ("56b", "56e"),
    ("78b", "78e"),
    ("34a", "34e"),
    ("56a", "56e"),
    ("78a", "78e"),
  )

  val edges_TINF_origins: Vector[String] = Vector("1D", "34D", "56D", "78D", "9D", "1C", "34C", "56C", "78C", "9C", "1AB", "70FE", "34B", "56B", "78B", "34A", "56A", "78A")

  val VS_inflow_edges: Vector[(String, String)] = Vector(("1wh", "1w"), ("1d", "1w"), ("1wc", "1w"), ("34d", "34w"), ("34c", "34w"), ("56d", "56w"), ("56c", "56w"), ("78d", "78w"), ("78c", "78w"), ("9h4", "78w"), ("1e", "34e"), ("34b", "34e"), ("34a", "34e"), ("56b", "56e"), ("56a", "56e"), ("56h", "56e"))
  val VS_outflow_edges: Vector[(String, String)] = Vector(("1wh", "nwh"), ("1d", "1h2"), ("1wc", "1c"), ("34d", "34D"), ("34c", "34C"), ("56d", "56D"), ("56c", "56C"), ("78d", "78D"), ("78c", "78C"), ("9h4", "9w"), ("1e", "1eh"), ("34b", "34B"), ("34a", "34A"), ("56b", "56B"), ("56a", "56A"), ("56h", "78e"))

  val areas: Map[Int, Vector[String]] = Map(0 -> Vector("9h4", "78d", "56d", "34d", "1d", "BAR", "1wh", "1wc", "KIOSK", "34c", "56c", "78c"), 1 -> Vector("56h", "56b", "34b", "1e", "34a", "56a"))


  // Exit links
  val exit_centroids_exit_edges: ListMap[String, (Vertex, Vertex)] = ListMap(
    "SW" -> (positions("sw"), positions("SW")),
    "NW" -> (positions("nw"), positions("NW")),
    "NWM" -> (positions("nwm"), positions("NWM")),
    "SE" -> (positions("se"), positions("SE")),
    "NEM" -> (positions("nem"), positions("NEM")),
    "N" -> (positions("nh"), positions("N")),
    "NE" -> (positions("neh1"), positions("NE"))
    //"NW": ("nww", "NW"), negligibly small
  )


  // linking track numbres to platform names
  val track_platform_dict: ListMap[Int, String] = ListMap(
    1 -> "1",
    3 -> "3/4",
    4 -> "3/4",
    5 -> "5/6",
    6 -> "5/6",
    7 -> "7/8",
    8 -> "7/8",
    9 -> "9",
    70 -> "70"
  )

  val centroid_platform_dict: ListMap[String, String] = ListMap(
    "1D" -> "1",
    "1C" -> "1",
    "70FE" -> "70",
    "1AB" -> "1",
    "34D" -> "3/4",
    "34C" -> "3/4",
    "34B" -> "3/4",
    "34A" -> "3/4",
    "56D" -> "5/6",
    "56C" -> "5/6",
    "56B" -> "5/6",
    "56A" -> "5/6",
    "78D" -> "7/8",
    "78C" -> "7/8",
    "78B" -> "7/8",
    "78A" -> "7/8",
    "9D" -> "9",
    "9C" -> "9"
  )

  // Structural components of Lausanne useful for Circos and other highly aggregated plots
  val structural_labels: Vector[String] = Vector("P1", "P34", "P56", "P78", "P9", "P70", "Metro", "North", "South", "Shops")

  //Dictionary that aggregates the centroids in these structural zones:
  val structural_centroids_dict: ListMap[String, Int] = ListMap("1D" -> 0, "1C" -> 0, "70FE" -> 5, "1AB" -> 0, "34D" -> 1, "34C" -> 1, "34B" -> 1, "34A" -> 1, "56D" -> 2, "56C" -> 2, "56B" -> 2, "56A" -> 2, "78D" -> 3, "78C" -> 3, "78B" -> 3, "78A" -> 3, "9D" -> 4, "9C" -> 4, "NW" -> 7, "NWM" -> 6, "NE" -> 7, "NEM" -> 6, "SW" -> 8, "SE" -> 8, "N" -> 7, "KIOSK" -> 9, "BAR" -> 9, "SHOP" -> 9)

  val ASE_edge_names_dict: ListMap[String, (Vertex, Vertex)] = ListMap(
    "ASE9ab_in" -> (positions("9h3"), positions("9w")),
    "ASE9cde_in" -> (positions("sw"), positions("9w")),
    "ASE9fgh_in" -> (positions("9h2"), positions("9w")),
    "ASE4_out" -> (positions("1c"), positions("1wc")),
    "ASE5a_in" -> (positions("nw"), positions("nwh")),
    "ASE10_in" -> (positions("nwm"), positions("nwh")),
    "ASE8_in" -> (positions("se"), positions("78e")),
    "ASE2_out" -> (positions("ne"), positions("1eh")),
    "ASE2de_out" -> (positions("nem"), positions("1eh")),
    "ASE3_in" -> (positions("nh"), positions("h")),
    "ASE1_in" -> (positions("neh1"), positions("neh2")),
    "ASE6_in" -> (positions("nww"), positions("1h2")),

    "ASE9ab_out" -> (positions("9h3"), positions("9c")),
    "ASE9cde_out" -> (positions("sw"), positions("SW")),
    "ASE9fgh_out" -> (positions("9h2"), positions("9h1")),
    "ASE4_in" -> (positions("1c"), positions("1h3")),
    "ASE5a_out" -> (positions("nw"), positions("NW")),
    "ASE10_out" -> (positions("nwm"), positions("NWM")),
    "ASE8_out" -> (positions("se"), positions("SE")),
    "ASE2_in" -> (positions("ne"), positions("neh2")),
    "ASE2de_in" -> (positions("nem"), positions("NEM")),
    "ASE3_out" -> (positions("nh"), positions("N")),
    "ASE1_out" -> (positions("neh1"), positions("NE")),
    "ASE6_out" -> (positions("nww"), positions("NW")),
  )


  val edges_sens_correction: Vector[(String, String)] = Vector(
    ("nw", "nwh"), // nwIn
    ("nwm", "nwh"), // nwmIn
    ("nh", "h"), // nhIn
    ("neh1", "neh2"), // neh1In

    ("nw", "NW"), // nwOut
    ("nwm", "NWM"), // nwmOut
    ("nh", "N"), // nhOut
    ("neh1", "NE"), // neh1Out

    // following links tentatively considered
    ("sw", "9w"), // swIn
    ("9h2", "9w"), // gh2In
    ("9h3", "9w"), // gh3In
    ("se", "78e"), // seIn

    ("sw", "SW"), // swOut
    ("9h2", "9h1"), // g9h2Out
    ("9h3", "9c"), // 9h3Out
    ("se", "SE") // seOut
  )



  val sector_dest_flow_fractions: Map[String, Double] = Map("A"-> 0.095, "B"-> 0.271, "C"-> 0.475, "D" -> 0.159)

  val centroid_platform_dest_flow_fractions: Map[String, Double] = Map("1D"-> 0.25,
    "1C"-> 0.5,
    "70FE"-> 1.0,
    "1AB"-> 0.25,
    "34D"-> sector_dest_flow_fractions("D"),
    "34C"-> sector_dest_flow_fractions("C"),
    "34B"-> sector_dest_flow_fractions("B"),
    "34A"-> sector_dest_flow_fractions("A"),
    "56D"-> sector_dest_flow_fractions("D"),
    "56C"-> sector_dest_flow_fractions("C"),
    "56B"-> sector_dest_flow_fractions("B"),
    "56A"-> sector_dest_flow_fractions("A"),
    "78D"-> sector_dest_flow_fractions("D"),
    "78C"-> sector_dest_flow_fractions("C"),
    "78B"-> sector_dest_flow_fractions("B"),
    "78A"-> sector_dest_flow_fractions("A"),
    "9D"-> 0.75,
    "9C"-> 0.25)


  val sales_per_minute: Map[String, Double] = Map("KIOSK"-> 1.0, "BAR"-> 1.0, "SHOP"-> 1.0)


  val edgesToFlurinIdx: Map[(String, String), Int] = Map(
    ("56w", "56c") -> 128, ("34a", "34A") -> 146, ("78b", "78B") -> 87, ("1h1", "1h2") -> 108, ("78c", "78w") -> 88, ("1e", "1eh") -> 80, ("34w", "34d") -> 157, ("nwh", "nw") -> 56, ("9d", "9D") -> 15, ("9C", "9c") -> 58, ("34c", "34C") -> 151, ("1D", "1h1") -> 24, ("neh2", "neh1") -> 98, ("78w", "78c") -> 124, ("1wh", "nwh") -> 3, ("kioskh", "34w") -> 5, ("neh2", "70fe") -> 100, ("1eh", "ne") -> 115, ("9h1", "9h2") -> 142, ("1h2", "1d") -> 92, ("nww", "1h2") -> 19, ("1w", "1d") -> 53, ("9d", "9h1") -> 14, ("neh2", "ne") -> 99, ("34e", "34b") -> 139, ("sw", "9w") -> 9, ("9w", "9h2") -> 40, ("SHOP", "9h1") -> 104, ("56B", "56b") -> 94, ("1h4", "1h3") -> 48, ("34B", "34b") -> 111, ("NW", "nw") -> 168, ("34c", "34w") -> 150, ("56d", "56w") -> 36, ("9h1", "SHOP") -> 141, ("KIOSK", "kioskh") -> 61, ("1h4", "1C") -> 49, ("78B", "78C") -> 26, ("9c", "9C") -> 11, ("34A", "34a") -> 110, ("sw", "SW") -> 8, ("1e", "34e") -> 81, ("78C", "78c") -> 28, ("1w", "kioskh") -> 50, ("nww", "NW") -> 20, ("78e", "78b") -> 77, ("BAR", "1w") -> 2, ("9w", "sw") -> 42, ("78a", "78A") -> 84, ("1eh", "1e") -> 114, ("56b", "56e") -> 39, ("1eh", "nem") -> 113, ("56h", "56e") -> 119, ("9h1", "9d") -> 143, ("NW", "nww") -> 167, ("1h3", "h") -> 105, ("56e", "56b") -> 68, ("nw", "NW") -> 103, ("1AB", "1ab") -> 162, ("56c", "56C") -> 33, ("56d", "56D") -> 35, ("1wc", "1w") -> 13, ("9c", "9h3") -> 10, ("kioskh", "1w") -> 7, ("ne", "1eh") -> 62, ("56e", "56h") -> 64, ("78A", "78a") -> 173, ("9h2", "9w") -> 164, ("78d", "78w") -> 71, ("34C", "34c") -> 112, ("56w", "56d") -> 129, ("34e", "34a") -> 137, ("se", "78e") -> 171, ("34w", "34c") -> 160, ("70fe", "70FE") -> 170, ("9w", "9h3") -> 41, ("h", "neh2") -> 153, ("56h", "78e") -> 118, ("nh", "N") -> 45, ("56c", "56w") -> 34, ("78c", "78C") -> 89, ("1h3", "1c") -> 106, ("34e", "1e") -> 135, ("1C", "1h4") -> 18, ("1h3", "1h4") -> 107, ("34a", "34e") -> 145, ("1h2", "1h1") -> 91, ("neh2", "h") -> 97, ("neh1", "neh2") -> 31, ("56C", "56c") -> 95, ("1d", "1h2") -> 59, ("70FE", "70fe") -> 155, ("56w", "78w") -> 130, ("34d", "34w") -> 134, ("56a", "56A") -> 21, ("78e", "78a") -> 75, ("se", "SE") -> 172, ("neh1", "NE") -> 32, ("78C", "78B") -> 27, ("neh2", "1ab") -> 101, ("9h3", "9c") -> 165, ("56a", "56e") -> 23, ("SW", "sw") -> 156, ("1c", "1wc") -> 79, ("SE", "se") -> 161, ("34d", "34D") -> 133, ("70fe", "neh2") -> 169, ("9h4", "9w") -> 132, ("56e", "56a") -> 66, ("nwh", "1wh") -> 57, ("78e", "56h") -> 72, ("1w", "BAR") -> 52, ("h", "nh") -> 152, ("56e", "34e") -> 69, ("9h2", "9h1") -> 163, ("NWM", "nwm") -> 93, ("1wc", "1c") -> 12, ("78a", "78e") -> 83, ("kioskh", "KIOSK") -> 6, ("nwh", "nwm") -> 55, ("nw", "nwh") -> 102, ("9h3", "9w") -> 166, ("NEM", "nem") -> 1, ("78B", "78b") -> 25, ("9h4", "78w") -> 131, ("1wh", "1w") -> 4, ("56D", "56d") -> 96, ("1ab", "1AB") -> 117, ("9w", "9h4") -> 43, ("1d", "1w") -> 60, ("1w", "1wh") -> 54, ("nem", "NEM") -> 47, ("34D", "34d") -> 0, ("9D", "9d") -> 125, ("56b", "56B") -> 37, ("NE", "neh1") -> 16, ("nwm", "NWM") -> 29, ("nem", "1eh") -> 46, ("56w", "34w") -> 127, ("1ab", "neh2") -> 116, ("34w", "kioskh") -> 159, ("78w", "56w") -> 122, ("ne", "neh2") -> 63, ("78b", "78e") -> 86, ("N", "nh") -> 120, ("78w", "78d") -> 121, ("78d", "78D") -> 70, ("78w", "9h4") -> 123, ("34e", "56e") -> 140, ("1h2", "nww") -> 90, ("1h1", "1D") -> 109, ("nwm", "nwh") -> 30, ("h", "1h3") -> 154, ("78D", "78d") -> 17, ("34b", "34B") -> 149, ("78e", "se") -> 73, ("56A", "56a") -> 126, ("34w", "56w") -> 158, ("nh", "h") -> 44, ("1w", "1wc") -> 51, ("34b", "34e") -> 148, ("1c", "1h3") -> 78
  )
  
  val routesToFlurinIdx: Map[(String, String), Int] = Map(
    ("56D","KIOSK") -> 242,
    ("56D","SW") -> 240,
    ("34A","78B") -> 236,
    ("34B","NEM") -> 219,
    ("1AB","34B") -> 182,
    ("78B","56A") -> 327,
    ("56A","70FE") -> 282,
    ("NEM","SE") -> 36,
    ("34C","56D") -> 212,
    ("78A","1AB") -> 333,
    ("56A","78B") -> 286,
    ("56B","78A") -> 277,
    ("56C","KIOSK") -> 257,
    ("SE","78B") -> 67,
    ("NW","9C") -> 12,
    ("1D","56C") -> 144,
    ("34A","70FE") -> 232,
    ("SW","NW") -> 45,
    ("78D","34C") -> 298,
    ("NW","BAR") -> 2,
    ("70FE","SE") -> 166,
    ("KIOSK","SW") -> 94,
    ("56A","SE") -> 280,
    ("1AB","NE") -> 177,
    ("KIOSK","34C") -> 99,
    ("78B","34A") -> 325,
    ("N","34A") -> 81,
    ("SE","78A") -> 68,
    ("78A","56B") -> 336,
    ("BAR","34C") -> 113,
    ("56C","NWM") -> 254,
    ("N","SHOP") -> 73,
    ("1AB","NEM") -> 178,
    ("56D","34C") -> 248,
    ("1C","BAR") -> 154,
    ("56D","78C") -> 250,
    ("56A","34A") -> 285,
    ("KIOSK","56D") -> 100,
    ("SE","34A") -> 64,
    ("56D","NWM") -> 239,
    ("1D","NW") -> 134,
    ("34C","NWM") -> 204,
    ("BAR","1D") -> 110,
    ("56A","NEM") -> 279,
    ("SHOP","56C") -> 129,
    ("78C","BAR") -> 308,
    ("70FE","34B") -> 169,
    ("34D","56C") -> 198,
    ("78C","34C") -> 313,
    ("70FE","9D") -> 175,
    ("N","56A") -> 85,
    ("9C","NWM") -> 355,
    ("KIOSK","1D") -> 96,
    ("SHOP","1C") -> 125,
    ("56C","SHOP") -> 259,
    ("78C","56C") -> 315,
    ("N","KIOSK") -> 71,
    ("9C","1D") -> 361,
    ("9D","56C") -> 351,
    ("NW","9D") -> 11,
    ("1D","BAR") -> 139,
    ("78D","SHOP") -> 294,
    ("34C","N") -> 206,
    ("56C","BAR") -> 258,
    ("9C","1C") -> 362,
    ("34B","N") -> 221,
    ("NWM","BAR") -> 15,
    ("78D","1D") -> 295,
    ("SE","34B") -> 63,
    ("34C","BAR") -> 208,
    ("9D","KIOSK") -> 342,
    ("1C","SHOP") -> 155,
    ("SHOP","78D") -> 130,
    ("NEM","78A") -> 44,
    ("KIOSK","34D") -> 98,
    ("78B","N") -> 321,
    ("SE","NEM") -> 59,
    ("NE","56B") -> 32,
    ("1C","78C") -> 161,
    ("34D","9C") -> 202,
    ("56C","1C") -> 261,
    ("34B","56A") -> 225,
    ("N","78C") -> 87,
    ("NEM","56B") -> 41,
    ("34C","78C") -> 215,
    ("BAR","1C") -> 111,
    ("34A","N") -> 231,
    ("1D","78C") -> 146,
    ("34B","1AB") -> 223,
    ("56C","9C") -> 267,
    ("9C","KIOSK") -> 358,
    ("1D","9C") -> 148,
    ("78B","70FE") -> 322,
    ("KIOSK","78C") -> 103,
    ("SW","34D") -> 50,
    ("78C","9C") -> 317,
    ("1D","78D") -> 145,
    ("NWM","KIOSK") -> 14,
    ("78C","56D") -> 314,
    ("SE","N") -> 60,
    ("1AB","78B") -> 186,
    ("34B","NE") -> 218,
    ("NE","34B") -> 30,
    ("1AB","34A") -> 183,
    ("34C","1C") -> 211,
    ("9D","34C") -> 349,
    ("NW","78D") -> 9,
    ("NE","78A") -> 35,
    ("34B","SE") -> 220,
    ("34D","1C") -> 196,
    ("9C","78C") -> 369,
    ("1C","9C") -> 163,
    ("34C","9D") -> 216,
    ("70FE","56B") -> 171,
    ("34D","NW") -> 188,
    ("NWM","56D") -> 21,
    ("78D","9C") -> 302,
    ("BAR","56D") -> 114,
    ("70FE","78A") -> 174,
    ("NE","34A") -> 31,
    ("78D","1C") -> 296,
    ("N","SW") -> 69,
    ("78A","SE") -> 330,
    ("34A","NE") -> 228,
    ("NW","78C") -> 10,
    ("34D","SHOP") -> 194,
    ("NWM","34C") -> 20,
    ("SW","34C") -> 51,
    ("34A","SE") -> 230,
    ("1C","NW") -> 149,
    ("SHOP","SW") -> 122,
    ("78B","NE") -> 318,
    ("34B","70FE") -> 222,
    ("56D","1D") -> 245,
    ("70FE","NEM") -> 165,
    ("34B","56B") -> 224,
    ("SE","NE") -> 58,
    ("NE","56A") -> 33,
    ("9D","SW") -> 340,
    ("34C","SHOP") -> 209,
    ("1D","9D") -> 147,
    ("56D","N") -> 241,
    ("NWM","34D") -> 19,
    ("N","BAR") -> 72,
    ("56A","78A") -> 287,
    ("78C","1D") -> 310,
    ("56D","34D") -> 247,
    ("SE","56A") -> 66,
    ("9D","SHOP") -> 344,
    ("34D","56D") -> 197,
    ("78C","SW") -> 305,
    ("NWM","56C") -> 22,
    ("78C","34D") -> 312,
    ("56C","NW") -> 253,
    ("SW","1C") -> 49,
    ("70FE","1AB") -> 168,
    ("9D","70FE") -> 347,
    ("BAR","9D") -> 118,
    ("34C","78D") -> 214,
    ("56A","34B") -> 284,
    ("78C","NW") -> 303,
    ("SHOP","56D") -> 128,
    ("SW","56C") -> 53,
    ("NWM","78D") -> 23,
    ("78D","NW") -> 288,
    ("9D","BAR") -> 343,
    ("N","34C") -> 79,
    ("N","1AB") -> 77,
    ("BAR","34D") -> 112,
    ("70FE","N") -> 167,
    ("56B","1AB") -> 273,
    ("N","56D") -> 82,
    ("1D","56D") -> 143,
    ("NEM","34A") -> 40,
    ("NE","70FE") -> 28,
    ("NW","56C") -> 8,
    ("NW","SW") -> 0,
    ("56D","SHOP") -> 244,
    ("78D","KIOSK") -> 292,
    ("SW","NWM") -> 46,
    ("NEM","78B") -> 43,
    ("SHOP","9D") -> 132,
    ("1D","KIOSK") -> 138,
    ("9D","NWM") -> 339,
    ("N","34D") -> 78,
    ("56B","34B") -> 274,
    ("N","70FE") -> 76,
    ("NW","34D") -> 5,
    ("56A","N") -> 281,
    ("56D","9D") -> 251,
    ("NW","56D") -> 7,
    ("KIOSK","78D") -> 102,
    ("N","9D") -> 90,
    ("78D","56D") -> 299,
    ("SHOP","1D") -> 124,
    ("SW","9C") -> 57,
    ("78B","56B") -> 326,
    ("34A","1AB") -> 233,
    ("78C","1C") -> 311,
    ("78B","SE") -> 320,
    ("N","56C") -> 83,
    ("1AB","56A") -> 185,
    ("70FE","56A") -> 172,
    ("78A","70FE") -> 332,
    ("56B","NEM") -> 269,
    ("34C","KIOSK") -> 207,
    ("78D","N") -> 291,
    ("BAR","78C") -> 117,
    ("1D","SHOP") -> 140,
    ("1C","KIOSK") -> 153,
    ("NW","KIOSK") -> 1,
    ("SW","1D") -> 48,
    ("34C","NW") -> 203,
    ("56C","SW") -> 255,
    ("SHOP","NW") -> 120,
    ("56B","78B") -> 276,
    ("34A","56A") -> 235,
    ("BAR","78D") -> 116,
    ("34A","NEM") -> 229,
    ("34D","N") -> 191,
    ("78A","34B") -> 334,
    ("SW","N") -> 47,
    ("34D","SW") -> 190,
    ("BAR","SW") -> 108,
    ("34B","78B") -> 226,
    ("NWM","9D") -> 25,
    ("SHOP","9C") -> 133,
    ("9C","34D") -> 364,
    ("1C","34D") -> 156,
    ("SW","78D") -> 54,
    ("KIOSK","56C") -> 101,
    ("NWM","SHOP") -> 16,
    ("78D","34D") -> 297,
    ("SE","1AB") -> 62,
    ("9D","78D") -> 352,
    ("78C","N") -> 306,
    ("NWM","9C") -> 26,
    ("KIOSK","9C") -> 105,
    ("1C","N") -> 152,
    ("NE","78B") -> 34,
    ("1AB","N") -> 180,
    ("NWM","1D") -> 17,
    ("78C","SHOP") -> 309,
    ("78A","NE") -> 328,
    ("70FE","78B") -> 173,
    ("78B","34B") -> 324,
    ("1C","SW") -> 151,
    ("78C","9D") -> 316,
    ("BAR","NWM") -> 107,
    ("56B","34A") -> 275,
    ("9D","56D") -> 350,
    ("56D","9C") -> 252,
    ("9C","70FE") -> 363,
    ("56C","78D") -> 264,
    ("56C","34C") -> 263,
    ("1D","NWM") -> 135,
    ("1D","34C") -> 142,
    ("1D","SW") -> 136,
    ("70FE","34A") -> 170,
    ("56D","BAR") -> 243,
    ("34D","KIOSK") -> 192,
    ("34D","BAR") -> 193,
    ("N","56B") -> 84,
    ("34D","1D") -> 195,
    ("N","78D") -> 86,
    ("NWM","78C") -> 24,
    ("KIOSK","NW") -> 92,
    ("9C","78D") -> 368,
    ("N","1D") -> 74,
    ("56D","NW") -> 238,
    ("78C","NWM") -> 304,
    ("NEM","1AB") -> 38,
    ("9C","NW") -> 354,
    ("34A","56B") -> 234,
    ("78B","1AB") -> 323,
    ("78D","BAR") -> 293,
    ("9D","34D") -> 348,
    ("34A","78A") -> 237,
    ("NW","SHOP") -> 3,
    ("1C","34C") -> 157,
    ("SHOP","34D") -> 126,
    ("SW","78C") -> 55,
    ("NEM","70FE") -> 37,
    ("78C","KIOSK") -> 307,
    ("56D","1C") -> 246,
    ("1AB","78A") -> 187,
    ("NE","SE") -> 27,
    ("9C","56C") -> 367,
    ("NEM","56A") -> 42,
    ("N","34B") -> 80,
    ("56B","70FE") -> 272,
    ("9D","78C") -> 353,
    ("SW","56D") -> 52,
    ("KIOSK","1C") -> 97,
    ("56B","SE") -> 270,
    ("34C","9C") -> 217,
    ("SE","56B") -> 65,
    ("9C","BAR") -> 359,
    ("34B","78A") -> 227,
    ("1C","9D") -> 162,
    ("9D","N") -> 341,
    ("78A","34A") -> 335,
    ("56A","NE") -> 278,
    ("SHOP","34C") -> 127,
    ("78B","NEM") -> 319,
    ("34D","78D") -> 199,
    ("KIOSK","NWM") -> 93,
    ("1C","56D") -> 158,
    ("NW","34C") -> 6,
    ("9C","N") -> 357,
    ("N","78A") -> 89,
    ("SHOP","NWM") -> 121,
    ("34D","9D") -> 201,
    ("78D","56C") -> 300,
    ("SHOP","N") -> 123,
    ("34C","SW") -> 205,
    ("34D","78C") -> 200,
    ("70FE","NE") -> 164,
    ("SE","70FE") -> 61,
    ("9D","NW") -> 338,
    ("56C","N") -> 256,
    ("9D","1C") -> 346,
    ("1D","34D") -> 141,
    ("BAR","56C") -> 115,
    ("NW","1D") -> 4,
    ("34D","NWM") -> 189,
    ("1D","N") -> 137,
    ("78A","N") -> 331,
    ("BAR","9C") -> 119,
    ("N","SE") -> 70,
    ("56B","NE") -> 268,
    ("56C","1D") -> 260,
    ("1AB","70FE") -> 181,
    ("9C","56D") -> 366,
    ("78A","56A") -> 337,
    ("1C","NWM") -> 150,
    ("KIOSK","N") -> 95,
    ("N","78B") -> 88,
    ("34C","1D") -> 210,
    ("56D","78D") -> 249,
    ("NWM","SW") -> 13,
    ("56C","9D") -> 266,
    ("56A","1AB") -> 283,
    ("KIOSK","9D") -> 104,
    ("78D","9D") -> 301,
    ("78D","NWM") -> 289,
    ("BAR","NW") -> 106,
    ("SHOP","78C") -> 131,
    ("1C","56C") -> 159,
    ("9C","34C") -> 365,
    ("1AB","56B") -> 184,
    ("SW","9D") -> 56,
    ("78A","NEM") -> 329,
    ("56C","34D") -> 262,
    ("9C","SW") -> 356,
    ("N","9C") -> 91,
    ("9D","1D") -> 345,
    ("BAR","N") -> 109,
    ("NWM","1C") -> 18,
    ("9C","SHOP") -> 360,
    ("70FE","9C") -> 176,
    ("56B","N") -> 271,
    ("NE","1AB") -> 29,
    ("NEM","34B") -> 39,
    ("1AB","SE") -> 179,
    ("1C","78D") -> 160,
    ("34C","56C") -> 213,
    ("78D","SW") -> 290,
    ("N","1C") -> 75,
    ("56C","78C") -> 265
  )

  val edgeCollection: ListMap[(Vertex, Vertex), MyRawEdge[Vertex]] = ListMap[(Vertex, Vertex), MyRawEdge[Vertex]]() ++
    (edgesSym ++ edgesSym.map(e => ((e._1._2, e._1._1), e._2)) ++ edgesAsym)
      .map(e => {
        (positions(e._1._1), positions(e._1._2)) -> new MyRawEdge(positions(e._1._1), positions(e._1._2), e._2)
      })

  val edgeIndices: ListMap[(Vertex, Vertex), Int] = ListMap[(Vertex, Vertex), Int]() ++ edgeCollection.keys.toVector.zipWithIndex

  // graph object for compouting the shortest paths
  val graph = new Graph(
    positions.values,
    edgeCollection.values
  )

  //computes all routes and subroutes
  val fullRoutes: ListMap[(Vertex, Vertex), (Double, List[Vertex])] = ListMap[(Vertex, Vertex), (Double, List[Vertex])]() ++ positions.values.flatMap(graph.getShortestPaths)

  val routes: ListMap[(Vertex, Vertex), (Double, List[Vertex])] = ListMap[(Vertex, Vertex), (Double, List[Vertex])]() ++ {
    for {
      a <- this.centroids
      b <- this.centroids
      if a != b && this.routesToFlurinIdx.get(a,b).isDefined
    } yield {
      (positions(a), positions(b)) -> fullRoutes((positions(a), positions(b)))
    }
  }

  val routesIndices: Map[(Vertex, Vertex), Int] = /*ListMap[(Vertex, Vertex), Int]() ++ routes.keys.map(k => k -> */this.routesToFlurinIdx.map(k => (this.positions(k._1._1), this.positions(k._1._2)) -> k._2)
  val routesIndicesReversed: Map[Int, (Vertex, Vertex)] = this.routesIndices.map(v => v._2 -> v._1)

  val subroutes_VS: ListMap[(Vertex, Vertex), (Double, List[Vertex])] = ListMap[(Vertex, Vertex), (Double, List[Vertex])]() ++ {
    for {
      a <- this.VS_east_nodes
      b <- this.VS_east_nodes
      if a != b} yield {
      (positions(a), positions(b)) -> fullRoutes((positions(a), positions(b)))
    }
  } ++ {
    for {
      a <- this.VS_west_nodes
      b <- this.VS_west_nodes
      if a != b} yield {
      (positions(a), positions(b)) -> fullRoutes((positions(a), positions(b)))
    }
  }

  val dist_routes_areas: ListMap[((Vertex, Vertex), Int), (Double, Double)] = ListMap[((Vertex, Vertex), Int), (Double, Double)]() ++ this.routes.values.flatMap(r => {
    val commonNodes: Map[Int, Vector[String]] = areas.collect { case (k: Int, v: Vector[String]) if v.intersect(r._2.map(_.name)).nonEmpty => k -> v.intersect(r._2.map(_.name)) }
    commonNodes.iterator.map(kv => {
      val tmp: Iterable[Double] = kv._2.map(a => fullRoutes(r._2.head, positions(a))._1)
      ((r._2.head, r._2.last), kv._1) -> (tmp.min, tmp.max)
    })
  })

  val TINFEdgesIndices: Map[(Vertex, Vertex), Int] = edges_TINF.zipWithIndex.map(kv => (positions(kv._1._1), positions(kv._1._1)) -> kv._2).toMap


  def route_orig_idx(route: (Vertex, Vertex)): Int = this.centroids.indexOf(route._1.name)
  def route_dest_idx(route: (Vertex, Vertex)): Int = this.centroids.indexOf(route._2.name)

  val routesFromCentroid: Map[String, Vector[Int]] = this.centroids
    .map(c => c -> this.routesIndices.filter(r => r._1._1.name == c).map(_._2).toVector).toMap

  val routesToCentroid: Map[String, Vector[Int]] = this.centroids
    .map(c => c -> this.routesIndices.filter(r => r._1._2.name == c).map(_._2).toVector).toMap
}

