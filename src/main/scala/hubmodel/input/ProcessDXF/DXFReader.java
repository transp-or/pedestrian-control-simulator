package org.kabeja;

import org.kabeja.dxf.DXFDocument;
import org.kabeja.parser.DXFParser;
import org.kabeja.parser.ParseException;
import org.kabeja.parser.Parser;
import org.kabeja.parser.ParserBuilder;

import java.io.FileInputStream;
import java.io.FileNotFoundException;

public class DXFReader {

    private DXFDocument _document;

    public DXFDocument getDXFDocument() {return _document;}

    public DXFReader(String fileName) {

        //parser
        Parser parser = ParserBuilder.createDefaultParser();

        try {
            FileInputStream file = new FileInputStream(fileName);
            try {
                parser.parse(file, DXFParser.DEFAULT_ENCODING);
            } catch (ParseException e) {
                e.printStackTrace();
            }
        } catch (FileNotFoundException e) {
            e.printStackTrace();
        }

        //get the document and the different layers
        this._document = parser.getDocument();
    }
}
