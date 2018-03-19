package org.kabeja;

import java.util.ArrayList;

public class Connections {
    String origin;
    ArrayList<String> conn = new ArrayList<>();
    private String connToString() {
        String str = "[";
        for (int i = 0; i < conn.size(); i++) {
            str += "\"" + conn.get(i).replace("{", "").replace("}", "") + "\"";
            if (i != conn.size()-1) {str += ",";}
        }
        str += "]";
        return str;
    }

    public Connections(String o) {
        this.origin = o;
    }

    public String toJSON() {
        return "{" +
                "\"node\": \"" + this.origin.replace("{", "").replace("}", "") + "\", " +
                "\"connected_to\": " + this.connToString() + "}";
    }
}
