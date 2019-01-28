package hubmodel.input.ProcessDXF;

import org.kabeja.dxf.helpers.Point;

import java.awt.geom.Path2D;
import java.math.BigDecimal;
import java.math.RoundingMode;

/**
 * Representation of a zone in the CAD file. These zones are used by the route choice model for computing the route
 * of the pedestrians. The zones must be orthogonal rectangles for th ehub model (for the sake of simplicity).
 */
class Zone {

    public static double round(double value, int places) {
        if (places < 0) throw new IllegalArgumentException();

        BigDecimal bd = new BigDecimal(value);
        bd = bd.setScale(places, RoundingMode.HALF_UP);
        return bd.doubleValue();
    }

    /**
     * Is the zone and OD
     */
    boolean isOD = true;

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
    String name = "";

    /**
     * Constructor. On creation, the Path2D object is created so that the assignment of the names can be performed.
     *
     * @param aIn a corner
     * @param bIn a corner
     * @param cIn a corner
     * @param dIn a corner
     */
    Zone(Point aIn, Point bIn, Point cIn, Point dIn) {

        // sets the four corner
        this.a = new Point(round(aIn.getX(), 3), round(aIn.getY(), 3), aIn.getZ());
        this.b = new Point(round(bIn.getX(), 3), round(bIn.getY(), 3), bIn.getZ());
        this.c = new Point(round(cIn.getX(), 3), round(cIn.getY(), 3), cIn.getZ());
        this.d = new Point(round(dIn.getX(), 3), round(dIn.getY(), 3), dIn.getZ());

        // builds the Path2D object
        polygon = new Path2D.Double();
        polygon.moveTo(this.a.getX(), this.a.getY());
        polygon.lineTo(this.b.getX(), this.b.getY());
        polygon.lineTo(this.c.getX(), this.c.getY());
        polygon.lineTo(this.d.getX(), this.d.getY());
        polygon.lineTo(this.a.getX(), this.a.getY());
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
    String toJSON(Double scale) {
        return "{" +
                "\"name\": \"" + this.name.replace("{", "").replace("}", "") + "\"," +
                "\"x\":" + 0.0 + "," +
                "\"y\":" + 0.0 + "," +
                "\"x1\":" + this.a.getX() * scale + "," +
                "\"y1\":" + this.a.getY() * scale + "," +
                "\"x2\":" + this.b.getX() * scale + "," +
                "\"y2\":" + this.b.getY() * scale + "," +
                "\"x3\":" + this.c.getX() * scale + "," +
                "\"y3\":" + this.c.getY() * scale + "," +
                "\"x4\":" + this.d.getX() * scale + "," +
                "\"y4\":" + this.d.getY() * scale + "," +
                "\"OD\": " + this.isOD +
                "}";
    }
}
