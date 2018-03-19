package org.kabeja;

import org.kabeja.dxf.helpers.Point;

public class Edge {

    Point a, b;
    String O, D;

    // constructor
    public Edge(Point a, Point b, String o, String d) {
        this.a = a;
        this.b = b;
        this.O = o;
        this.D = d;
    }

    @Override
    public String toString() {
        return "[ O=" + this.O + ", D=" + this.D  + "]";
    }

}
