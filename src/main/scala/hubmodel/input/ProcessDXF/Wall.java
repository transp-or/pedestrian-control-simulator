package org.kabeja;

import org.kabeja.dxf.helpers.Point;

public class Wall {
    Point a, b;
    int type = 0;

    // constructor
    public Wall(Point a, Point b) {
        this.a = a;
        this.b = b;
        if (this.a.getZ() != this.b.getZ()){
            throw new IllegalArgumentException("heights are different at each end of wall");
        }
        this.type = (int)this.a.getZ();
    }


    @Override
    public String toString() {
        return "[(" + a.getX() + ", " + a.getY() +"), (" + b.getX() + ", " + b.getY() + "), type=" +this.type + "]";
    }

    public String toJSON() {
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