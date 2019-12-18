package hubmodel.io.output

import hubmodel.supply.graph.MyEdge
import java.nio.charset.Charset
import java.nio.file.Files
import java.nio.file.Paths

import hubmodel.supply.continuous.Wall
import tools.cells.{Rectangle, Vertex}


package object tikz {

  def writeEdgesAsTikz(file: String, edges: Set[MyEdge], vertices: Iterable[Vertex], walls: Iterable[Wall]): Unit = {

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
      "\\PreviewEnvironment{tikzpicture}\n" +
      "\\setlength\\PreviewBorder{5pt}\n\n" +
      "\\input{/home/nicholas/TexConfig/Tikz.tex}\n\n" +
      "\\begin{document}\n\n" +
      "% -------------------------- Styles -----------------------------\n" +
      "\\tikzset{edge/.style={line width=1pt, draw=black,decoration={markings,mark=at position 0.4 with {\\arrow[scale=3]{stealth}}}}}\n" +
      "\\begin{tikzpicture}")


    // writes vertices to tikz
    // TODO fix this problem for creating tikz data
    //vertices.foreach(v => writer.write(s"\\fill[black!10!white] (${v.xMin}, ${v.yMin}) rectangle (${v.xMax}, ${v.yMax});\n"))

    // writes walls to tikz
    walls.foreach(w =>  writer.write(s"\\draw[line width=0.35cm, cap=rect] (${w.startPoint.X},${w.startPoint.Y}) -- (${w.endPoint.X},${w.endPoint.Y});\n"))

    // write the edges as tikz lines
    edges.foreach(e => {writer.write(s"\\draw[-, edge, postaction={decorate}] (${e.startVertex.center.X},${e.startVertex.center.Y}) -- (${e.endVertex.center.X},${e.endVertex.center.Y});\n")})


    // writes tex closing
    writer.write("\\end{tikzpicture}\n\\end{document}")

    // closes the file
    writer.close()

  }

}
