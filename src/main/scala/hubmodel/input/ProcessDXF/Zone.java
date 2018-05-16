package hubmodel.input.ProcessDXF;

import org.kabeja.dxf.helpers.Point;

import java.awt.geom.Path2D;

/**
 * Representation of a zone in the CAD file. These zones are used by the route choice model for computing the route
 * of the pedestrians. The zones must be orthogonal rectangles for th ehub model (for the sake of simplicity).
 */
class Zone {

    /**
     * Corners of the zone. Must be a rectangle for the hub model.
     */
    private Point a, b, c, d;

    /**
     * As the question "is a point inside me ?" needs to be answered, the Path2D object is used.
     */
    Path2D.Double polygon;

    /**
     * name of the zone
     */
    String name;

    /**
     * Constructor. On creation, the Path2D object is created so that the assignment of the names can be performed.
     *
     * @param a a corner
     * @param b a corner
     * @param c a corner
     * @param d a corner
     */
    Zone(Point a, Point b, Point c, Point d) {

        // sets the four corner
        this.a = a;
        this.b = b;
        this.c = c;
        this.d = d;

        // builds the Path2D object
        polygon = new Path2D.Double();
        polygon.moveTo(a.getX(), a.getY());
        polygon.lineTo(b.getX(), b.getY());
        polygon.lineTo(c.getX(), c.getY());
        polygon.lineTo(d.getX(), d.getY());
        polygon.lineTo(a.getX(), a.getY());
    }


    /**
     * Converts this object to JSON format for the hub model. Example:
     * {
     * "name": "a",
     * "x": 0.0,
     * "y": 0.0,
     * "x1": 47.71234866828081,
     * "y1": 247.8312348668281,
     * "x2": 47.71234866828078,
     * "y2": 188.6055690072639,
     * "x3": 113.466828087167,
     * "y3": 188.6055690072639,
     * "x4": 113.466828087167,
     * "y4": 247.8312348668281
     * }
     *
     * @return String with the object as JSON
     */
    String toJSON() {
        return "{" +
                "\"name\": \"" + this.name.replace("{", "").replace("}", "") + "\"," +
                "\"x\":" + 0.0 + "," +
                "\"y\":" + 0.0 + "," +
                "\"x1\":" + this.a.getX() + "," +
                "\"y1\":" + this.a.getY() + "," +
                "\"x2\":" + this.b.getX() + "," +
                "\"y2\":" + this.b.getY() + "," +
                "\"x3\":" + this.c.getX() + "," +
                "\"y3\":" + this.c.getY() + "," +
                "\"x4\":" + this.d.getX() + "," +
                "\"y4\":" + this.d.getY() +
                "}";
    }
}
