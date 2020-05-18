import org.xml.sax.InputSource;
import org.xml.sax.XMLReader;
import org.xml.sax.helpers.XMLReaderFactory;

import sax.user.MySaxHandler;

public class MainSax {

    private static String INPUT_FILE = "data.xml";

    public static void main(String[] args) {
        try {
            // Create parser instance
            XMLReader parser = XMLReaderFactory.createXMLReader();

            // Create input stream from source XML document
            InputSource source = new InputSource(INPUT_FILE);

            // Set our custom content handler for handling SAX events
            parser.setContentHandler(new MySaxHandler());
            
            // Process input data
            parser.parse(source);
            
        } catch (Exception e) {
            
            e.printStackTrace();
            
        }
    }
}