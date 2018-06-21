package nomad.operational;


import com.vividsolutions.jts.geom.Coordinate;
import hubmodel.supply.continuous.Wall;

public class InfluenceAreaReturnObsData {
    public Wall obstacle;
    public Coordinate coordinate;


    public InfluenceAreaReturnObsData(Wall wall, Coordinate coordinate) {
        super();
        this.obstacle = wall;
        this.coordinate = coordinate;
    }
}