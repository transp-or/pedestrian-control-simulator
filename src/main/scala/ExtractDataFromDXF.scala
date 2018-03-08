import org.kabeja.dxf.DXFConstants
import org.kabeja.parser.{DXFParser, ParserBuilder}

object ExtractDataFromDXF extends App {

  val parser = ParserBuilder.createDefaultParser
  parser.parse("path/file.dxf", DXFParser.DEFAULT_ENCODING)
  val doc = parser.getDocument
  val layer = doc.getDXFLayer("layer_name")
  val arcs = layer.getDXFEntities(DXFConstants.ENTITY_TYPE_CIRCLE)
}