// package dom.user;
package user; //required in the assignment

import java.math.BigInteger;
import java.util.ArrayList;
import org.w3c.dom.*;

public class MyDomTransformer {
   
    /**
     * Process document tree from the user perspective
     * @param doc Document to be parsed
     */
    public void transform(Document doc) {
        addOwnedStadionsTotalValue(doc);
        contactInfoNormalization(doc);
    }

    /**
     * Adds a new child element <owned-stadions-total-value> to <club> element.
     * Newly added child element contains sum of prices of all owned stadions
     * @param doc Document to be parsed
     */
    private void addOwnedStadionsTotalValue(Document doc) {
        NodeList clubs = doc.getElementsByTagName("club");
        for(int i = 0; i < clubs.getLength(); ++i) {
            Element club = (Element) clubs.item(i);

            //Making list of ids of owned stadions
            ArrayList<String> ownedStadionsId = new ArrayList<String>();
            NodeList ownedStadionsRef = club.getElementsByTagName("owned-stadion");
            for(int j = 0; j < ownedStadionsRef.getLength(); ++j) {
                Element ownedStadionRef = (Element) ownedStadionsRef.item(j);
                ownedStadionsId.add(ownedStadionRef.getAttribute("refId"));
            }

            //Getting prices of stadions whose id is owned by the club
            BigInteger totalValue = new BigInteger("0");
            for(int j = 0; j < ownedStadionsId.size(); ++j) {
                Element stadion = (Element) doc.getElementById(ownedStadionsId.get(j));
                Element price = (Element) (stadion.getElementsByTagName("price").item(0));
                String priceS = price.getTextContent().trim();
                try {
                    BigInteger priceBI = new BigInteger(priceS);
                    totalValue = totalValue.add(priceBI);
                } 
                catch(NumberFormatException e) { }
            }

            //Adding a new element with total value to the club element
            Element total = doc.createElement("owned-stadions-total-value");
            total.setTextContent(totalValue.toString());
            club.appendChild(total);
        }
    }
    
    /**
     * Capitalizes the first letter and trims textContent of <first-name> and <last-name> elements of each <person>
     * Additionally removes <prefix> element if it (after trimming) contains value "+420" which is considered to be default
     * @param doc Document to be parsed
     */ 
    private void contactInfoNormalization(Document doc) {
       NodeList people = doc.getElementsByTagName("person");
       for(int i = 0; i < people.getLength(); ++i) {
           Element person = (Element) people.item(i);

           //Normalizing <first-name> and <last-name>
           Element firstName = (Element) person.getElementsByTagName("first-name").item(0);
           Element lastName = (Element) person.getElementsByTagName("last-name").item(0);
           normalizeElement(firstName);
           normalizeElement(lastName);

           //Removing <prefix> if it exists and (after trimming) contains value "+420"
           NodeList prefix = person.getElementsByTagName("prefix");
           if(prefix.getLength() == 1) {
                Element p = (Element) prefix.item(0);
                String pValue = p.getTextContent().trim();
                if(pValue.equals("+420")) {
                    Element telephone = (Element) p.getParentNode();
                    telephone.removeChild(p);
                }
           }


       }
    }
    /**
     * Capitalizes the first letter and trims textContent of an element
     * @param e Element to be processed
     */
    private void normalizeElement(Element e) {
        String trimmed = e.getTextContent().trim();
        String capitalized = trimmed.substring(0, 1).toUpperCase() + trimmed.substring(1);
        e.setTextContent(capitalized);
    }
}
