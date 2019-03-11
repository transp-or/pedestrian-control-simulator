package hubmodel.io.output

import java.io.{BufferedWriter, File, FileWriter}

import hubmodel.ped.PedestrianTrait
import hubmodel.supply.graph.Stop2Vertex
import hubmodel.tools.Time
import hubmodel.tools.TimeNumeric.mkOrderingOps
import hubmodel.{GroupID, VertexID}
import myscala.math.stats.{Quantiles, computeQuantiles}
import play.api.libs.json.{JsValue, Json, Writes}


package object TRANSFORM {

  implicit class PopulationProcessingTRANSFORM[T <: PedestrianTrait](pop: Iterable[T]) {

    def computeTT4TRANSFORM(quantiles: Seq[Double], startTime: Time, endTime: Time, fileName: String, startDay: String = "1970-01-01", endDay: String = "2100-12-31"): Unit = {
      val res: collection.mutable.Map[(String, String), collection.mutable.ArrayBuffer[Double]] = collection.mutable.Map()
      pop.foreach(p => {
        if (p.entryTime >= startTime || p.exitTime <= endTime) {
          res.getOrElseUpdate((p.origin.name, p.finalDestination.name), collection.mutable.ArrayBuffer()).append(p.travelTime.value.toDouble)
        }
      })
      val file = new File(fileName)
      val bw = new BufferedWriter(new FileWriter(file))
      bw.write(Json.prettyPrint(Json.toJson(
        res.map(kv => ODWithQuantiles(kv._1._1, kv._1._2, startDay + " " + startTime.asReadable, endDay + " " + endTime.asReadable, computeQuantiles(quantiles)(kv._2)))
      )))
      bw.close()
    }
  }

  implicit class PopulationSummaryProcessingTRANSFORM(pop: Iterable[(VertexID, VertexID, Double, Double, Double)]) {

    case class ZoneName(hub: VertexID, stop: String, group: Int)

    def computeTT4TRANSFORM(quantiles: Seq[Double], startTime: Time, endTime: Time, fileName: String, stop2Vertex: Stop2Vertex, startDay: String = "1970-01-01", endDay: String = "2100-12-31"): Iterable[(String, String, Iterable[Double])] = {

      def stopGrouping(vertexID: VertexID): GroupID = {
        stop2Vertex.grouping4TRANSFORM.indexWhere(groups => groups.contains(vertexID))
      }

      val reversedMap: Map[String, String] = stop2Vertex.stop2Vertices.flatMap(kv => kv._2.map(v => v -> kv._1.toString))

      def vertices2Stops(vertexID: VertexID): String = {
        reversedMap.getOrElse(vertexID, vertexID.toString)
      }

      val popRenamed: Iterable[(ZoneName, ZoneName, Double, Double, Double)] = pop
        .map(p => (ZoneName(p._1, vertices2Stops(p._1), stopGrouping(p._1)), ZoneName(p._2, vertices2Stops(p._2), stopGrouping(p._2)), p._3, p._4, p._5))

      val popRenamedFiltered: Iterable[(ZoneName, ZoneName, Double, Double, Double)] = if (stop2Vertex.stop2Vertices.nonEmpty) {
        popRenamed.filter(p => stop2Vertex.stop2Vertices.keySet.map(_.toString).contains(p._1.stop) && stop2Vertex.stop2Vertices.keySet.map(_.toString).contains(p._2.stop))
      } else {
        popRenamed
      }

      val res: Iterable[(String, String, Iterable[Double])] = popRenamedFiltered
        .filter(p => p._4 >= startTime.value || p._5 <= endTime.value)
        .groupBy(p => (if (p._1.group == -1) {
          p._1.hub
        } else {
          p._1.group
        }, if (p._2.group == -1) {
          p._2.hub
        } else {
          p._2.group
        }))
        .map(kv => kv._1 -> kv._2.groupBy(g => (g._1.stop, g._2.stop)))
        .flatMap(g => g._2.map(ig => (ig._1._1, ig._1._2, g._2.flatMap(v => v._2.map(_._3)))))


      val file = new File(fileName)
      val bw = new BufferedWriter(new FileWriter(file))
      bw.write(Json.prettyPrint(Json.toJson(
        res.map(kv => {
          ODWithQuantiles(kv._1, kv._2, startDay + " " + startTime.asReadable, endDay + " " + endTime.asReadable, computeQuantiles(quantiles)(kv._3.toSeq))
        })
      )))
      bw.close()

      res
    }
  }


  private case class ODWithQuantiles(O: String, D: String, start: String, end: String, q: Quantiles[Double])

  private implicit def ODQuantilesWrites: Writes[ODWithQuantiles] = new Writes[ODWithQuantiles] {
    def writes(d: ODWithQuantiles): JsValue = {
      Json.obj(
        "o" -> d.O,
        "d" -> d.D,
        "start_timestamp" -> d.start,
        "end_timestamp" -> d.end,
        "quantiles" -> d.q.quantiles,
        "values" -> d.q.values,
        "sample_size" -> d.q.sampleSize
      )
    }
  }
}
