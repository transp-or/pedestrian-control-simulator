package hubmodel.io.output

import java.nio.charset.Charset
import java.nio.file.{Files, Paths}

import hubmodel.control.ControlDevices
import hubmodel.supply.continuous.Wall
import hubmodel.supply.graph.{MyEdge, MyEdgeLevelChange}
import tools.cells.{Circle, Rectangle, Vertex}


package object tikz {

  def writeEdgesAsTikz(file: String, edges: Set[MyEdge], vertices: Iterable[Vertex], walls: Iterable[Wall], devices: ControlDevices): Unit = {

    // create file for writing
    val writer = Files.newBufferedWriter(Paths.get(file), Charset.forName("UTF-8"))

    // writes tex header
    writer.write("" +
      "\\documentclass[border = 2pt]{article}\n\n" +
      "\\usepackage{enumitem}\n" +
      "\\usepackage{tikz}\n" +
      "\\usetikzlibrary{shapes}\n" +
      "\\usetikzlibrary{shapes.geometric}\n" +
      "\\usetikzlibrary{shapes.arrows}\n" +
      "\\usetikzlibrary{arrows}\n" +
      "\\usetikzlibrary{arrows.meta}\n" +
      "\\usetikzlibrary{shadows}\n" +
      "\\usetikzlibrary{decorations.text}\n" +
      "\\usepackage{amsmath,bm,times}\n" +
      "\\usetikzlibrary{calc,intersections}\n" +
      "\\usetikzlibrary{decorations.markings}\n" +
      "\\usepackage[active,tightpage]{preview}\n" +
      "\\usetikzlibrary{patterns}" +
      "\\PreviewEnvironment{tikzpicture}\n" +
      "\\setlength\\PreviewBorder{5pt}\n\n" +
      "%\\input{/home/nicholas/TexConfig/Tikz.tex}\n\n" +
      "\\begin{document}\n\n" +
      "% -------------------------- Styles -----------------------------\n" +
      "\\tikzset{edge/.style={line width=1pt, draw=black,decoration={markings,mark=at position 0.4 with {\\arrow[scale=3]{stealth}}}}}\n" +
      "\\tikzset{levelchange/.style={line width=1pt, dash pattern={on 1pt off 10pt}, draw=black,decoration={markings,mark=at position 0.4 with {\\arrow[scale=3]{stealth}}}}}\n" +
      "\\begin{tikzpicture}")


    // writes vertices to tikz
    // TODO fix this problem for creating tikz data

    vertices.collect{
      case r: Rectangle => writer.write(s"\\fill[black!10!white] (${r.xMin}, ${r.yMin}) rectangle (${r.xMax}, ${r.yMax});\n")
      case c: Circle => writer.write(s"\\fill[black!10!white] ${c.center} circle (${c.radius});\n")
    }

    // writes walls to tikz
    walls.foreach(w =>  writer.write(s"\\draw[line width=0.35cm, cap=rect] (${w.startPoint.X},${w.startPoint.Y}) -- (${w.endPoint.X},${w.endPoint.Y});\n"))

    // write the edges as tikz lines
    edges.collect{
      case e: MyEdgeLevelChange => writer.write(s"\\draw[-, levelchange, postaction={decorate}] (${e.startVertex.center.X},${e.startVertex.center.Y}) -- (${e.endVertex.center.X},${e.endVertex.center.Y});\n")
      case e: MyEdge => writer.write(s"\\draw[-, edge, postaction={decorate}] (${e.startVertex.center.X},${e.startVertex.center.Y}) -- (${e.endVertex.center.X},${e.endVertex.center.Y});\n")
    }

    devices.amws.foreach(amw => {
      writer.write(s"\\draw[pattern=vertical lines, pattern color=black] (${amw.walls.head.startPoint.X},${amw.walls.head.startPoint.Y}) rectangle (${amw.walls.last.endPoint.X},${amw.walls.last.endPoint.Y});\n")
      amw.walls.foreach(w => writer.write(s"\\draw[line width=0.35cm, cap=rect] (${w.startPoint.X},${w.startPoint.Y}) -- (${w.endPoint.X},${w.endPoint.Y});\n"))
    })

    // writes tex closing
    writer.write("\\end{tikzpicture}\n\\end{document}")

    // closes the file
    writer.close()

  }

}
