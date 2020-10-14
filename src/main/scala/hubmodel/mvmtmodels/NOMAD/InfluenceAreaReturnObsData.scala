package hubmodel.mvmtmodels.NOMAD

import hubmodel.supply.continuous.Wall
import myscala.math.vector.Vector2D

/*
public class InfluenceAreaReturnObsData {
    public Wall obstacle;
    public Coordinate coordinate;


    public InfluenceAreaReturnObsData(Wall wall, Coordinate coordinate) {
        super();
        this.obstacle = wall;
        this.coordinate = coordinate;
    }
}
 */

class InfluenceAreaReturnObsData(val obstacle: Wall, val coordinate: Vector2D)