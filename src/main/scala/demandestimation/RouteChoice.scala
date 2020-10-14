package demandestimation

import demandestimation.network.NetworkLausanne
import hubmodel.demand.transit.{TrainWithExchangeVolumes, Vehicle}
import tools.Time
import tools.TimeNumeric.mkOrderingOps
import tools.cells.Vertex

class RouteChoice(parameters: DemandEstimationParameters, network: NetworkLausanne) {

  type DestinationFlows = Vector[(String, Double)]


  /** Creates the flow heading towrads a platform
    *
    * @param timetable
    * @return
    */
  def generate_platform_dest_flow(timetable: Vector[Vehicle]): Map[String, Double] = {

    timetable.collect { case train: TrainWithExchangeVolumes if train.dep.isDefined && train.dep.get >= Time(parameters.startT) && train.dep.get <= Time(parameters.endT) => {
      (network.platformsMap(train.track), train.boardingPassengers.get)
    }
    }.groupBy(_._1).view.mapValues(group => group.map(_._2).sum).toMap

  }


  /** Rxeturns cumulative destination flows at centroids over time ordered by centroids_dict
    *
    * @param measurementData
    * @param platformDestinationFlows
    * @return
    */
  def generate_dest_flows(measurementData: MeasurementData, timetable: Vector[Vehicle]): DestinationFlows = {

    val platformDestinationFlows: Map[String, Double] = generate_platform_dest_flow(timetable)

    network.centroids.map(c => {
      if (network.Centroids_Platform.contains(c)) {
        //"platform"
        c ->  platformDestinationFlows.find(k => {
          k._1.contains(c.replaceAll("[A-Za-z]", ""))
        }).getOrElse(("", 0.0))._2 * network.centroid_platform_dest_flow_fractions(c)
      }
      else if (network.Centroids_Entrance_Exit.contains(c)) {
        //"entrance"
        c -> measurementData.ASEData.find(v => v._1 == network.exit_centroids_exit_edges(c)).get._2.sum
      }
      else {
        //"shop"
        c -> network.sales_per_minute(c) * (parameters.endT - parameters.startT) / 60.0
      }
    }
    )
  }


  /** Returns pedestrian fractions
    *
    * @param orig_type
    * @param dest_type
    */
  def ped_type_fraction(orig_type: String, dest_type: String): Double = {


    if (orig_type == "platform" && dest_type == "non-platform") {
      parameters.beta_p2np
    } else if (orig_type == "platform" && dest_type == "platform") {
      1.0 - parameters.beta_p2np

    } else if (orig_type == "non-platform" && dest_type == "platform") {
      parameters.beta_np2p

    } else if (orig_type == "non-platform" && dest_type == "non-platform") {
      1.0 - parameters.beta_np2p
    } else {
      throw new Exception("This case shoudl not happen !\r" + orig_type + "\r" + dest_type)
    }
  }

  type OriginFlowByDestinationType = Map[(Vertex, String), Double]

  /** OD flows creation, origin part
    *
    * @param network
    * @param dest_flows
    * @return
    */
  def origin_flow_by_destination_type(dest_flows: DestinationFlows): OriginFlowByDestinationType = {

    network.routes.iterator.map(r => {
      val dest_type: String = if (network.Centroids_Platform.contains(r._1._2.name)) {
        "platform"
      }
      else {
        "non-platform"
      }
      ((r._1._1, dest_type), dest_flows.find(_._1 == r._1._2.name).get._2)
    }).toVector.groupBy(_._1).view.mapValues(v => v.map(_._2).sum).toMap

  }

  /** Returns vector of route split fractions ordered by routes_dict
    *
    * @param dest_flows
    * @return
    */
  def compute_route_splits(dest_flows: DestinationFlows): Map[(Vertex, Vertex), Double] = {

    val originFLowsByDestType: OriginFlowByDestinationType = origin_flow_by_destination_type(dest_flows)

    network.routes.iterator.map(r => {
      val dest_type: String = if (network.Centroids_Platform.contains(r._1._2.name)) {
        "platform"
      }
      else {
        "non-platform"
      }
      val orig_type: String = if (network.Centroids_Platform.contains(r._1._1.name)) {
        "platform"
      }
      else {
        "non-platform"
      }

      r._1 -> ped_type_fraction(orig_type, dest_type) * dest_flows.find(_._1 == r._1._2.name).get._2 / originFLowsByDestType((r._1._1, dest_type))

    }).toMap

  }

  /** Quasi-static route split fractions
    *
    * @param dest_flows
    * @return
    */
  def route_split_GLS(dest_flows: DestinationFlows): Vector[(Int, Int, Double)] = {

    val route_splits = compute_route_splits(dest_flows)

    (for {
      r <- network.routes
      rp <- network.routesFromCentroid(r._1._1.name)
    } yield {
      val coeff = {
        if (network.routesIndices(r._1) == rp) {
          1.0 - route_splits(network.routesIndicesReversed(rp))
        }
        else {
          -route_splits(network.routesIndicesReversed(rp))
        }
      }

      parameters.intervals.indices.map(tInt => {
        (network.routesIndices(r._1) + tInt * network.routes.size, rp + tInt * network.routes.size, coeff)
      })
    }).flatten.toVector

  }

  /** Platform/non-platform aggragated cumulative route split fractions
    *
    * @param dest_flows
    * @return
    */
  def agg_cum_route_split_GLS(dest_flows: DestinationFlows): Vector[(Int, Int, Double)] = {

    network.centroids.flatMap(c => {
      val orig_type: String = if (network.Centroids_Platform.contains(c)) {
        "platform"
      }
      else {
        "non-platform"
      }
      val split_to_np: Double = ped_type_fraction(orig_type, "non-platform")
      network.routesFromCentroid(c).flatMap(r => {
        val dest_type: String = if (network.Centroids_Platform.contains(network.routesIndicesReversed(r)._2.name)) {
          "platform"
        }
        else {
          "non-platform"
        }
        val coeff: Double = if (dest_type == "non-platform") {
          1 - split_to_np
        } else if (dest_type == "platform") {
          -split_to_np
        } else {
          throw new Exception("This case should not happen")
        }

        parameters.intervals.indices.map(tInt => {
          (network.centroids.indexWhere(_ == c), r + tInt * network.routes.size, coeff)
        })
      })
    })
  }

  /** Departures from platforms over time
    *
    * @param estim_param
    * @param network
    * @param dest_flows
    * @return
    */
  def platform_centroid_dep_flow(dest_flows: DestinationFlows): (Vector[(Int, Int, Double)], Vector[(Int, Double)]) = {

    val matrices = for {
      c <- network.Centroids_Platform.zipWithIndex
    } yield {
      (
        for {
          r <- network.routesToCentroid(c._1)
          t <- parameters.intervals.indices
        } yield {
          (c._2, t * network.routes.size + r, 1.0)
        },
        (c._2, dest_flows.find(_._1 == c._1).get._2)
      )
    }

    (matrices.flatMap(_._1), matrices.map(_._2))
  }


  /** Sales over time
    *
    * @param estim_param
    * @param network
    * @param dest_flows
    * @return
    */
  def sales_data_GLS(dest_flows: DestinationFlows): (Vector[(Int, Int, Double)], Vector[(Int, Double)])  = {

    val matrices = for {
      s <- network.Centroids_Shop.zipWithIndex
    } yield {
      (
        (for {
          r <- network.routesFromCentroid(s._1)
          t <- parameters.intervals.indices
        } yield {
          (s._2, t * network.routes.size + r, 1.0)
        }).toVector ++ (for {
          r <- network.routesToCentroid(s._1)
          t <- parameters.intervals.indices
        } yield {
          (s._2 + network.Centroids_Shop.size, t * network.routes.size + r, 1.0)
        }).toVector,
        Vector((s._2, dest_flows.find(_._1 == s._1).get._2), (s._2 + network.Centroids_Shop.size, dest_flows.find(_._1 == s._1).get._2))
      )
    }

    (matrices.flatMap(_._1), matrices.flatMap(_._2))

  }

}
