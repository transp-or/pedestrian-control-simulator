import hubmodel.input.ProcessDXF.DXFReaderHubModel;

public class ProcessDXFForHubModel {

    public static void main(String[] args) {

        String fileName = "denhaag_w_network.dxf";
        String wallLayerID = "walls-all";
        String zoneLayerID = "zones";
        String graphLayerID = "network";

        DXFReaderHubModel dxf = new DXFReaderHubModel(fileName, wallLayerID, zoneLayerID, graphLayerID);
        dxf.writeWallsToFile("walls-test.json");
        dxf.writeGraphToFile("graph-test.json");
    }

}