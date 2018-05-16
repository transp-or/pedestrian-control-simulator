package hubmodel.input.ProcessDXF;

import java.util.ArrayList;

/**
 * Representation of a connection between one node and all connecting nodes. This can be seen as a connectivity
 * object between nodes. On creation the name of the current node is provided, and the connecting nodes must
 * be filled in later on.
 */
class Connections {

    /**
     * ID of this connection
     */
    String origin;

    /**
     * List of node ID connected to this node. These connections are directed.
     */
    ArrayList<String> conn = new ArrayList<>();

    /**
     * Builds the list of connecting nodes in JSON format. This method is used by the toJSON method. An example of
     * the output is the following, for this array: (a,b,c,d) -> ["a","b","c","d"]
     *
     * @return String containing the connections
     */
    private String connToString() {
        String str = "[";
        for (int i = 0; i < conn.size(); i++) {
            str += "\"" + conn.get(i).replace("{", "").replace("}", "") + "\"";
            if (i != conn.size() - 1) {
                str += ",";
            }
        }
        str += "]";
        return str;
    }

    /**
     * Constructor, taking the name of the current node as argument. There is no need to use this object outside
     * of the package, hence made package private.
     *
     * @param o name of the current node
     */
    Connections(String o) {
        this.origin = o;
    }

    /**
     * Writes the connection object to JSON in the format for the hub model. Example:
     * <p>
     * {
     * "node": "a",
     * "connectivity": ["e","b","c","d"]
     * }
     *
     * @return String with the connection as JSON.
     */
    String toJSON() {
        return "{" +
                "\"node\": \"" + this.origin.replace("{", "").replace("}", "") + "\", " +
                "\"connected_to\": " + this.connToString() + "}";
    }
}
