package hubmodel.io.input.ProcessDXF;

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
	 * Type of connection:
	 * - bidirectional: BI_DIR
	 * - unidirectional flat: UNI_DIR
	 * - unidirectional stairs up: UNI_DIR_UP
	 * - unidirectional stairs down UNI_DIR_DOWN
	 * - change level: LEVEL_CHANGE
	 */
	int type;

	/**
	 * Constructor. Takes the physical locations and the names of the origin and destination nodes as arguments.
	 *
	 * @param aIn start node physical position
	 * @param bIn end node physical position
	 * @param o   origin node name
	 * @param d   destination node name
	 */
	Edge(Point aIn, Point bIn, String o, String d, int type) {
		this.a = aIn;
		this.b = bIn;
		this.O = o;
		this.D = d;
		this.type = type;
	}
}
