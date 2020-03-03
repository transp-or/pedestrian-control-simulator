import hubmodel.io.input.ProcessDXF.DXFReaderHubModel;

public class ProcessDXFForHubModel {

	public static void main(String[] args) {

		String fileName = "three-platforms/three-platforms-two-corridors.dxf";
		String wallLayerID = "walls";
		String zoneLayerID = "zones";
		String graphLayerID = "graph";
		Double scalingFactor = 1.0 / 1000.0;

		DXFReaderHubModel dxf = new DXFReaderHubModel(fileName, wallLayerID, zoneLayerID, graphLayerID);
		dxf.writeWallsToFile("three-platforms/three-platforms-two-corridors-walls.json", scalingFactor);
		dxf.writeGraphToFile("three-platforms/three-platforms-two-corridors-graph.json", scalingFactor);
	}
}