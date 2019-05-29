import hubmodel.io.input.ProcessDXF.DXFReaderHubModel;

public class ProcessDXFForHubModel {

	public static void main(String[] args) {

		String fileName = "den-haag/denhaag_w_network.dxf";
		String wallLayerID = "walls-all";
		String zoneLayerID = "zones";
		String graphLayerID = "network";
		Double scalingFactor = 1.0 / 1000.0;

		DXFReaderHubModel dxf = new DXFReaderHubModel(fileName, wallLayerID, zoneLayerID, graphLayerID);
		dxf.writeWallsToFile("den-haag/walls-test.json", scalingFactor);
		dxf.writeGraphToFile("den-haag/graph-test.json", scalingFactor);
	}
}