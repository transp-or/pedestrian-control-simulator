package hubmodel.HubOutput.Writers

import java.time.LocalDateTime
import java.time.format.DateTimeFormatter

import hubmodel.input.infrastructure.{NodeID, ODPair}
import myscala.math.stats.Quantiles
import play.api.libs.json.{JsValue, Json, Writes}

/**
  * Created by nicholas on 3/31/17.
  */
object JSON {

  case class ODQuantiles(start: LocalDateTime, end: LocalDateTime, O: String, D: String, q: Quantiles[Double])

  implicit val ODQuantilesWrites = new Writes[ODQuantiles] {
    def writes(d: ODQuantiles): JsValue = {
      Json.obj(
        "o" -> d.O,
        "d" -> d.D,
        "start_timestamp" -> d.start.format(DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss")),
        "end_timestamp" -> d.end.format(DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss")),
        "quantiles" -> d.q.quantiles,
        "values" -> d.q.values,
        "sample_size" -> d.q.sampleSize
      )
    }
  }

  case class TTbyODQuantiles(location: String, od_tt_quantiles: Iterable[ODQuantiles])
  implicit val TTbyODQuantilesWrites: Writes[TTbyODQuantiles] = Json.writes[TTbyODQuantiles]

}


