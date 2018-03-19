package org.kabeja;

import org.kabeja.dxf.*;

import java.io.IOException;
import java.nio.charset.Charset;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;

public class DXFReaderHubModel extends DXFReader {

    private DXFLayer wallLayer;
    private DXFLayer zoneLayer;
    private DXFLayer graphLayer;

    public DXFReaderHubModel(String fileName, String wallLayerName, String zoneLayerName, String graphLayerName) {
        super(fileName);

        this.wallLayer = this.getDXFDocument().getDXFLayer(wallLayerName);
        this.zoneLayer = this.getDXFDocument().getDXFLayer(zoneLayerName);
        this.graphLayer = this.getDXFDocument().getDXFLayer(graphLayerName);
    }

    public void writeWallsToFile(String fileName) {

        // Reads all entities which can be used to create walls.
        List<DXFPolyline> lwpl_w = this.wallLayer.getDXFEntities(DXFConstants.ENTITY_TYPE_LWPOLYLINE);
        List<DXFPolyline> pl_w = this.wallLayer.getDXFEntities(DXFConstants.ENTITY_TYPE_POLYLINE);
        List<DXFLine> l_w = this.wallLayer.getDXFEntities(DXFConstants.ENTITY_TYPE_LINE);

        // initiliaze container for Wall objects
        List<Wall> walls =  new ArrayList<>();

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

        String str = "{\n" +
                "  \"location\": \"lausanne\",\n" +
                "  \"sublocation\": \"PIW\",\n" +
                "  \"walls\": [";

        for (int i =0; i < walls.size(); i++) {
            str += walls.get(i).toJSON();
            if (i != walls.size()-1) {
                str += ",";
            }
        }
        str += "]}";

        ArrayList<String> strA = new ArrayList<String>();
        strA.add(str);

        Path file = Paths.get(fileName);
        try { Files.write(file, strA , Charset.forName("UTF-8")); }
        catch (IOException e) {e.printStackTrace();}
    }

    public void writeGraphToFile(String fileName) {

        List<DXFPolyline> lwpl_z = this.zoneLayer.getDXFEntities(DXFConstants.ENTITY_TYPE_LWPOLYLINE);
        List<DXFPolyline> pl_z = this.zoneLayer.getDXFEntities(DXFConstants.ENTITY_TYPE_POLYLINE);
        List<DXFMText> names_z = this.zoneLayer.getDXFEntities(DXFConstants.ENTITY_TYPE_MTEXT);
        List<Zone> zones =  new ArrayList<>();
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
                zonesNames.add(names_z.get(i).getText());
                for (int j = 0; j < zones.size(); j++) {
                    if (zones.get(j).polygon.contains(names_z.get(i).getInsertPoint().getX(), names_z.get(i).getInsertPoint().getY())){
                        zones.get(j).name = names_z.get(i).getText();
                    }
                }
            }
        }

        List<DXFLine> l_g = this.graphLayer.getDXFEntities(DXFConstants.ENTITY_TYPE_LINE);
        List<Edge> edges =  new ArrayList<>();


        if (l_g != null) {
            for (int i = 0; i < l_g.size(); i++) {
                String o = "";
                String d = "";
                for (int j = 0; j < zones.size(); j++) {
                    if (zones.get(j).polygon.contains(l_g.get(i).getStartPoint().getX(), l_g.get(i).getStartPoint().getY())){
                        o = zones.get(j).name;
                    }
                    if (zones.get(j).polygon.contains(l_g.get(i).getEndPoint().getX(), l_g.get(i).getEndPoint().getY())){
                        d = zones.get(j).name;
                    }
                }
                edges.add(new Edge(l_g.get(i).getStartPoint(), l_g.get(i).getEndPoint(), o, d));
            }
        }


        List<Connections> connections =  new ArrayList<>();
        for (String n: zonesNames) {
            connections.add(new Connections(n));
        }

        for (Edge e: edges) {
            for (Connections c: connections) {
                if (c.origin.compareTo(e.O) == 0) {
                    c.conn.add(e.D);
                }
            }
        }

        String str = "{\"location\": \"lausanne\", \"sublocation\": \"test\",\"nodes\":[ ";


        for (int i =0; i < zones.size(); i++) {
            str += zones.get(i).toJSON();
            if (i != zones.size()-1) {
                str += ",";
            }
        }

        str += "], \"connectivity\": [";

        for (int i =0; i < connections.size(); i++) {
             str += connections.get(i).toJSON();
            if (i != connections.size()-1) {
                str += ",";
            }
        }
        str += "], \"flow_gates\": [], \"controlled_areas\": [], \"binary_gates\": [], \"flow_separators\": []}";

        ArrayList<String> strA = new ArrayList<String>();
        strA.add(str);
        Path file = Paths.get(fileName);
        try { Files.write(file, strA , Charset.forName("UTF-8")); }
        catch (IOException e) {e.printStackTrace();}

    }


    }
