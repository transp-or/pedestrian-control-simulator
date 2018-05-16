package hubmodel.input.ProcessDXF;

import org.kabeja.dxf.DXFDocument;
import org.kabeja.parser.DXFParser;
import org.kabeja.parser.ParseException;
import org.kabeja.parser.Parser;
import org.kabeja.parser.ParserBuilder;

import java.io.FileInputStream;
import java.io.FileNotFoundException;

/**
 * Parent class used for all DXF parsers.
 */
class DXFReader {

    /**
     * Storage of the document. Private as must no be modified
     */
    private DXFDocument _document;

    /**
     * Getter method for the DXFDocument
     *
     * @return DXFDocuement
     */
    DXFDocument getDXFDocument() {
        return _document;
    }

    /**
     * Constructor. First checks that the file can be opened, then opens it and reads the contents into a
     * DXFDocument object.
     *
     * @param fileName file to read
     */
    DXFReader(String fileName) {

        // parser
        Parser parser = ParserBuilder.createDefaultParser();

        try {
            // opens the file
            FileInputStream file = new FileInputStream(fileName);
            try {
                // parses the file
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
