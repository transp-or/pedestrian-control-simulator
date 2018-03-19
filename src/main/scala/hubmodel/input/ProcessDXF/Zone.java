package org.kabeja;

import org.kabeja.dxf.helpers.Point;

import java.awt.geom.Path2D;

public class Zone {
    Point a,b,c,d;
    Path2D.Double polygon;
    String name;

    // constructor
    public Zone(Point a, Point b, Point c, Point d) {
        this.a = a;
        this.b = b;
        this.c = c;
        this.d = d;

        polygon = new Path2D.Double();
        polygon.moveTo(a.getX(), a.getY());
        polygon.lineTo(b.getX(), b.getY());
        polygon.lineTo(c.getX(), c.getY());
        polygon.lineTo(d.getX(), d.getY());
        polygon.lineTo(a.getX(), a.getY());
    }

    @Override
    public String toString() {
        return "[" + this.name + ", (" + a.getX() + ", " + a.getY() + "), ("  + b.getX() + ", " + b.getY() + "), (" + c.getX() + ", " + c.getY() + "), (" + d.getX() + ", " + d.getY() + ")]";
    }

    public String toJSON() {
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
