package hubmodel.input.ProcessDXF;

import org.kabeja.dxf.helpers.Point;

/**
 * Representation of a physical wall ni the CAD file. These objects will be used by the microscopic
 * pedestrian simulator fas boundaries. In the CAD file, the height of each point is used to fill the type property.
 */
class Wall {

    /**
     * Start and end of the line representing a wall.
     */
    private Point a, b;

    /**
     * Indicator whether the wall is part of the outside shell or not.
     * - 0 means outer shell
     * - 1 means inner wall
     */
    private int type = 0;

    /**
     * Constructor. The start and end points are the only required arguments. The heights are read to fix the type,
     * if they do not match an IllegalArgumentException is thrown.
     *
     * @param a start point
     * @param b end point
     */
    Wall(Point a, Point b) {
        this.a = a;
        this.b = b;
        if (this.a.getZ() != this.b.getZ()) {
            throw new IllegalArgumentException("heights are different at each end of wall! w=" + this.a + ", " + this.b);
        }
        this.type = (int) this.a.getZ();
    }

    /**
     * Converts the object to the JSON format used by the hub model. Example:
     * <p>
     * {
     * "comment": "",
     * "x1": 0.0,
     * "y1": 0.0,
     * "x2": 1.0,
     * "y2": 1.0,
     * "type": 0
     * }
     *
     * @return String with the object as JSON.
     */
    String toJSON() {
        return "{" +
                "\"comment\": \"\", " +
                "\"x1\":" + this.a.getX() + "," +
                "\"y1\":" + this.a.getY() + "," +
                "\"x2\":" + this.b.getX() + "," +
                "\"y2\":" + this.b.getY() + "," +
                "\"type\":" + this.type +
                "}";
    }
}