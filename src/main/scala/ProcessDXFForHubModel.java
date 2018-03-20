import hubmodel.input.ProcessDXF.DXFReaderHubModel;

public class ProcessDXFForHubModel {

    public static void main(String[] args) {

        String fileName = "hub_test.dxf";
        String wallLayerID = "walls";
        String zoneLayerID = "zones";
        String graphLayerID = "graph";

        DXFReaderHubModel dxf = new DXFReaderHubModel(fileName, wallLayerID, zoneLayerID, graphLayerID);
        dxf.writeWallsToFile("walls-test.json");
        dxf.writeGraphToFile("graph-test.json");
    }

}