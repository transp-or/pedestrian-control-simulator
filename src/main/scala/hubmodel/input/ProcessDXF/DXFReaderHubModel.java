package hubmodel.input.ProcessDXF;

import org.kabeja.dxf.*;

import java.io.IOException;
import java.nio.charset.Charset;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;

import static hubmodel.input.ProcessDXF.EdgeTypes.*;
import static org.apache.commons.lang3.RandomStringUtils.randomAlphabetic;



/**
 * Parser for CAD files for the hub model. Extends the {@link DXFReader} class with methods for reading specific
 * layers and writing the contents in the JSON format for the hub model. The DXF file must be saved as: R2013 ASCII
 * <p>
 * ==Usage==
 * {{{
 * String fileName = "hub_test.dxf";
 * String wallLayerID = "walls";
 * String zoneLayerID = "zones";
 * String graphLayerID = "graph";
 * <p>
 * DXFReaderHubModel dxf = new DXFReaderHubModel(fileName, wallLayerID, zoneLayerID, graphLayerID);
 * dxf.writeWallsToFile("walls-test.json");
 * dxf.writeGraphToFile("graph-test.json");
 * }}}
 */
public class DXFReaderHubModel extends DXFReader {


    /**
     * Layer containing the walls. Set on construction.
     */
    private DXFLayer wallLayer;

    /**
     * Layer containing the zones. Set on construction.
     */
    private DXFLayer zoneLayer;

    /**
     * Layer containing the graph. Set on construction.
     */
    private DXFLayer graphLayer;

    /**
     * Constructor. The file name with the names of the three layers must be passed as arguments. On construction,
     * the layers are set.
     *
     * @param fileName       String with the name of the DXF file.
     * @param wallLayerName  String with the name of the wall layer.
     * @param zoneLayerName  String with the name of the zone layer.
     * @param graphLayerName String with the name of the grpah layer.
     */
    public DXFReaderHubModel(String fileName, String wallLayerName, String zoneLayerName, String graphLayerName) {
        super(fileName);

        this.wallLayer = this.getDXFDocument().getDXFLayer(wallLayerName);
        this.zoneLayer = this.getDXFDocument().getDXFLayer(zoneLayerName);
        this.graphLayer = this.getDXFDocument().getDXFLayer(graphLayerName);
    }

    /**
     * Collects all the walls on the specific layer, then writes them to JSON text file.
     * <p>
     * Three type of objects are scanned: {@link DXFConstants.ENTITY_TYPE_LWPOLYLINE},
     * {@link DXFConstants.ENTITY_TYPE_POLYLINE} and {@link DXFConstants.ENTITY_TYPE_LINE}. The polylines are then split
     * into individual {@link Wall}s, while the plain lines are directly converted to {@link Wall}. All the walls are
     * then written as JSON to the file passed as argument.
     *
     * @param fileName
     */
    public void writeWallsToFile(String fileName, Double scale) {

        // Reads all entities which can be used to create walls.
        List<DXFPolyline> lwpl_w = this.wallLayer.getDXFEntities(DXFConstants.ENTITY_TYPE_LWPOLYLINE);
        List<DXFPolyline> pl_w = this.wallLayer.getDXFEntities(DXFConstants.ENTITY_TYPE_POLYLINE);
        List<DXFLine> l_w = this.wallLayer.getDXFEntities(DXFConstants.ENTITY_TYPE_LINE);

        // initiliaze container for Wall objects
        List<Wall> walls = new ArrayList<>();

        // Process LWPOLYLINE objects
        if (lwpl_w != null) {
            for (int j = 0; j < lwpl_w.size(); j++) {
                for (int i = 0; i < lwpl_w.get(j).getVertexCount() - 1; i++) {
                    walls.add(new Wall(lwpl_w.get(j).getVertex(i).getPoint(), lwpl_w.get(j).getVertex(i + 1).getPoint()));
                }
            }
        }

        // Process POLYLINE objects
        if (pl_w != null) {
            for (int j = 0; j < pl_w.size(); j++) {
                for (int i = 0; i < pl_w.get(j).getVertexCount() - 1; i++) {
                    walls.add(new Wall(pl_w.get(j).getVertex(i).getPoint(), pl_w.get(j).getVertex(i + 1).getPoint()));
                }
            }
        }

        // Process LINE objects
        if (l_w != null) {
            for (int i = 0; i < l_w.size(); i++) {
                walls.add(new Wall(l_w.get(i).getStartPoint(), l_w.get(i).getEndPoint()));
            }
        }

        // initilize string
        String str = "{\n" +
                "  \"location\": \"lausanne\",\n" +
                "  \"sublocation\": \"PIW\",\n" +
                "  \"walls\": [";

        // appends all wall objects
        for (int i = 0; i < walls.size(); i++) {
            str += walls.get(i).toJSON(scale);
            if (i != walls.size() - 1) {
                str += ",";
            }
        }

        // closes JSON
        str += "]}";

        // Converts the string to an Array for writing
        ArrayList<String> strA = new ArrayList<String>();
        strA.add(str);

        // Writes file
        Path file = Paths.get(fileName);
        try {
            Files.write(file, strA, Charset.forName("UTF-8"));
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    /**
     * Collects and combines the zones and graph before writing them to a file.
     * <p>
     * On the zone layer, {@link DXFConstants.ENTITY_TYPE_LWPOLYLINE} and {@link DXFConstants.ENTITY_TYPE_POLYLINE}
     * objects are collected and {@link Zone} objects are built from them. Secondly, all
     * {@link DXFConstants.ENTITY_TYPE_MTEXT} objects are also collectd and used to name the nodes. The matching scheme
     * is the following: for each {@link DXFConstants.ENTITY_TYPE_MTEXT}, associate the text to the zone in which it
     * is located.
     * <p>
     * On the graph layer, only {@link DXFConstants.ENTITY_TYPE_LINE} objects are searched for. Each of these is
     * converted to an {@link Edge} object. Once this is done, {@link Connections} objects are built from them.
     * <p>
     * Finally, the zones and connections are written as JSON.
     *
     * @param fileName
     */
    public void writeGraphToFile(String fileName, Double scale) {

        List<DXFPolyline> lwpl_z = this.zoneLayer.getDXFEntities(DXFConstants.ENTITY_TYPE_LWPOLYLINE);
        List<DXFPolyline> pl_z = this.zoneLayer.getDXFEntities(DXFConstants.ENTITY_TYPE_POLYLINE);
        List<DXFMText> names_z = this.zoneLayer.getDXFEntities(DXFConstants.ENTITY_TYPE_MTEXT);
        List<Zone> zones = new ArrayList<>();
        HashSet<String> zonesNames = new HashSet<>();


        if (lwpl_z != null) {
            for (int j = 0; j < lwpl_z.size(); j++) {
                zones.add(new Zone(
                                lwpl_z.get(j).getVertex(0).getPoint(),
                                lwpl_z.get(j).getVertex(1).getPoint(),
                                lwpl_z.get(j).getVertex(2).getPoint(),
                                lwpl_z.get(j).getVertex(3).getPoint()
                        )
                );

            }
        }

        if (pl_z != null) {
            for (int j = 0; j < pl_z.size(); j++) {
                zones.add(new Zone(
                                pl_z.get(j).getVertex(0).getPoint(),
                                pl_z.get(j).getVertex(1).getPoint(),
                                pl_z.get(j).getVertex(2).getPoint(),
                                pl_z.get(j).getVertex(3).getPoint()
                        )
                );

            }
        }

        if (names_z != null) {
            for (int i = 0; i < names_z.size(); i++) {
                System.out.print(names_z.get(i).getText());
                if (names_z.get(i).getText().compareTo("") != 0) {zonesNames.add(names_z.get(i).getText());}
                for (int j = 0; j < zones.size(); j++) {
                    if (zones.get(j).polygon.contains(names_z.get(i).getInsertPoint().getX(), names_z.get(i).getInsertPoint().getY())) {
                        zones.get(j).name = names_z.get(i).getText();
                    }
                }
            }
        }

        for (int i = 0; i < zones.size(); i++) {
                if (zones.get(i).name.compareTo("") == 0) {
                    zones.get(i).name = randomAlphabetic(10);
                    zonesNames.add(zones.get(i).name);
                }
            zonesNames.add(zones.get(i).name);
        }

        List<DXFLine> l_g = this.graphLayer.getDXFEntities(DXFConstants.ENTITY_TYPE_LINE);
        List<Edge> edges = new ArrayList<>();

        List<Edge> levelChanges = new ArrayList<>();



        if (l_g != null) {
            for (int i = 0; i < l_g.size(); i++) {
                String o = "";
                String d = "";
                for (int j = 0; j < zones.size(); j++) {
                    if (zones.get(j).polygon.contains(l_g.get(i).getStartPoint().getX(), l_g.get(i).getStartPoint().getY())) {
                        o = zones.get(j).name;
                    }
                    if (zones.get(j).polygon.contains(l_g.get(i).getEndPoint().getX(), l_g.get(i).getEndPoint().getY())) {
                        d = zones.get(j).name;
                    }
                }
                if (o.compareTo("") == 0) {
                    System.out.print("missing zone at " + o + ", " + l_g.get(i).getStartPoint()+ "\n");
                }
                if (d.compareTo("") == 0){
                    System.out.print("missing zone at " + d + ", " +  l_g.get(i).getEndPoint() + "\n");
                }

                int edgeType = 0;
                if (l_g.get(i).getLineWeight() == 0) {
                    edgeType = BI_DIR;
                    edges.add(new Edge(l_g.get(i).getStartPoint(), l_g.get(i).getEndPoint(), o, d, edgeType));
                    edges.add(new Edge(l_g.get(i).getEndPoint(), l_g.get(i).getStartPoint(), d, o, edgeType));
                } else if (l_g.get(i).getLineWeight() == 13) {
                    edgeType = UNI_DIR;
                    edges.add(new Edge(l_g.get(i).getStartPoint(), l_g.get(i).getEndPoint(), o, d, edgeType));
                }
                else if (l_g.get(i).getLineWeight() == 5) {
                    edgeType = UNI_DIR_UP;
                    levelChanges.add(new Edge(l_g.get(i).getStartPoint(), l_g.get(i).getEndPoint(), o, d, edgeType));
                }
                else if (l_g.get(i).getLineWeight() == 9) {
                    edgeType = UNI_DIR_DOWN;
                    levelChanges.add(new Edge(l_g.get(i).getStartPoint(), l_g.get(i).getEndPoint(), o, d, edgeType));
                }
            }
        }

        // Converts edges to connectivity for standard edges
        List<Connections> connections = new ArrayList<>();
        for (String n : zonesNames) {
            connections.add(new Connections(n));
        }

        for (Edge e : edges) {
            for (Connections c : connections) {
                if (c.origin.compareTo(e.O) == 0) {
                    c.conn.add(e.D);
                }
            }
        }


        // Converts edges to connectivity for level changes
        HashSet<String> zonesNamesLevelChanges = new HashSet<>();
        for (Edge lc : levelChanges) {
            zonesNamesLevelChanges.add(lc.O);
        }

        System.out.print(zonesNamesLevelChanges);

        List<Connections> levelChangeConnections = new ArrayList<>();
        for (String n : zonesNamesLevelChanges) {
            levelChangeConnections.add(new Connections(n));
        }

        for (Edge e : levelChanges) {
            for (Connections c : levelChangeConnections) {
                if (c.origin.compareTo(e.O) == 0) {
                    c.conn.add(e.D);
                }
            }
        }

        String str = "{\"location\": \"lausanne\", \"sublocation\": \"test\",\"nodes\":[ ";


        for (int i = 0; i < zones.size(); i++) {
            str += zones.get(i).toJSON(scale);
            if (i != zones.size() - 1) {
                str += ",";
            }
        }

        str += "], \"connectivity\": [";

        for (int i = 0; i < connections.size(); i++) {
            str += connections.get(i).toJSON();
            if (i != connections.size() - 1) {
                str += ",";
            }
        }
        str += "], \"flow_gates\": [], \"controlled_areas\": [], \"binary_gates\": [], \"flow_separators\": [], \"moving_walkways\": [], \"alternate_graphs\": [], \"connectivity_level_change\": [";

        for (int i = 0; i < levelChangeConnections.size(); i++) {
            str += levelChangeConnections.get(i).toJSON();
            if (i != levelChangeConnections.size() - 1) {
                str += ",";
            }
        }

        str += "]" + "}";

        ArrayList<String> strA = new ArrayList<String>();
        strA.add(str);
        Path file = Paths.get(fileName);
        try {
            Files.write(file, strA, Charset.forName("UTF-8"));
        } catch (IOException e) {
            e.printStackTrace();
        }

    }


}
