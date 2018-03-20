package hubmodel.input.ProcessDXF;

import org.kabeja.dxf.helpers.Point;

/**
 * Representation of a line in the CAD file between two zones. Package private as no usage for this object outside
 * of the DXF parser.
 */
class Edge {

    /**
     * Start and end points of the edge.
     */
    private Point a, b;

    /**
     * Names of the origin and destination nodes.
     */
    String O, D;

    /**
     * Constructor. Takes the physical locations and the names of the origin and destination nodes as arguments.
     *
     * @param a start node physical position
     * @param b end node physical position
     * @param o origin node name
     * @param d destination node name
     */
    Edge(Point a, Point b, String o, String d) {
        this.a = a;
        this.b = b;
        this.O = o;
        this.D = d;
    }
}
