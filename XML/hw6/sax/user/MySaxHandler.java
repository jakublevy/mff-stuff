// package sax.user;
package user; //required in the assignment

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.Serializable;
import java.util.ArrayList;

import org.xml.sax.Attributes;
import org.xml.sax.Locator;
import org.xml.sax.SAXException;
import org.xml.sax.helpers.DefaultHandler;

public class MySaxHandler extends DefaultHandler {

    //Flags indicating whether we are parsing element appearing in the name of variable
    boolean insidePossessions = false;
    boolean insidePrice = false;
    boolean insideMotto = false;
    boolean insideFirstName =false;
    boolean insideLastName =false;



    //List of number of owned posessessions for each club
    ArrayList<Integer> ownedPossessionsCount = new ArrayList<Integer>();

    //Variable counting the number of possessions for currently parsed club
    Integer currentPossessionsCount = 0;


    //Finding cheapest and the most expensive stadion
    Double minStadionPrice = Double.MAX_VALUE;
    String minStadionId = "";
    Double maxStadionPrice = Double.MIN_VALUE;
    String maxStadionId = "";
    String currentStadionId = "";

    //Finding oldest and youngest motto
    Motto currentMotto;
    Motto oldestMotto;
    Motto newestMotto;

    //For checking whether people are sorted
    boolean sortedByLastNameThenFirstName = true;
    Person previous; 
    Person current;
    
    // Reference to the locator
    Locator locator;
    
    /**
     * Stores reference to the locator
     * @param Locator locator Location in the source file
     */
    @Override
    public void setDocumentLocator(Locator locator) {
        this.locator = locator;
    }
    
    /**
     * Document beginning handler
     * @throws SAXException
     */
    @Override
    public void startDocument() throws SAXException {
        // ...
        oldestMotto = null;
        newestMotto = null;
        currentMotto = new Motto();
        current = new Person();
    }
    
    /**
     * Document end handler
     * @throws SAXException
     */
    @Override
    public void endDocument() throws SAXException {
        System.out.println("Average number of owned possessions by a single club is " + avg(ownedPossessionsCount));
        System.out.println("Cheapest stadion Id = " + minStadionId + ", it has a value of " + minStadionPrice);
        System.out.println("The most expensive stadion Id = " + maxStadionId + ", it has a value of " + maxStadionPrice);

        if(oldestMotto != null)
            System.out.println("Oldest motto is used since " + oldestMotto.getYear() + ". Its raw value is: " + oldestMotto.getText());
        else 
            System.out.println("Input does not contain a motto with since attribute");

        if(newestMotto != null)
            System.out.println("Youngest motto is used since " + newestMotto.getYear() + ". Its raw value is: " + newestMotto.getText());

        if(sortedByLastNameThenFirstName) 
            System.out.println("People are sorted on last name, then first name basis");
        else
            System.out.println("People are NOT sorted on last name, then first name basis"); 

    }
    
    /**
     * Element beginning handler
     * @param uri URI of the element namespace (empty when no namespace)
     * @param localName Local name of the element (never empty)
     * @param qName Qualified name
     * @param atts Attributes
     * @throws SAXException
     */
    @Override
    public void startElement( String uri, String localName, String qName, Attributes atts) throws SAXException { 
        if(insidePossessions) 
            ++currentPossessionsCount; //we found <owned-stadion>
 

        if(qName.equals("possessions")) {
            insidePossessions = true;
            currentPossessionsCount = 0;
        }

        if(qName.equals("stadion")) 
            currentStadionId = atts.getValue("id");
 

        if(qName.equals("price")) 
            insidePrice = true;
 

        if(qName.equals("motto")) {
            String since = atts.getValue("since");
            if(since != null) { //since argument is not required, it can be null
                if(tryParseInt(since)) {
                    currentMotto.setYear(Integer.parseInt(since));
                    insideMotto = true;
                }
            }
        }

        if(qName.equals("first-name")) 
            insideFirstName = true;
 
        if(qName.equals("last-name"))
            insideLastName = true;
    }
    
    /**
     * Element end handler
     * @param uri URI of the element namespace (empty when no namespace)
     * @param localName Local name of the element (never empty)
     * @param qName Qualified name
     * @throws SAXException
     */
    @Override
    public void endElement( String uri, String localName, String qName) throws SAXException {
        if(qName.equals("possessions")) {
            insidePossessions = false;

            //We have already processed all <possessions> of a single club
            ownedPossessionsCount.add(currentPossessionsCount);
        }

        if(qName.equals("price")) 
            insidePrice = false;
 
        if(qName.equals("motto")) 
            insideMotto = false;

        if(qName.equals("first-name"))
            insideFirstName = false;

        if(qName.equals("last-name"))
            insideLastName = false;
 
    }
    
    /**
     * Character data handler
     *  - Text content may be delivered within multiple events, not just one
     * @param chars Array with character data
     * @param start Index of the start position in the array
     * @param length Number of characters to read from the array
     * @throws SAXException
     */
    @Override
    public void characters( char[] chars, int start, int length) throws SAXException {
        String text = new String(chars, start, length).trim();
        if(text.equals("")) 
            return;

        if(insidePossessions) 
            ++currentPossessionsCount; //we found a possession like "traktor", "parkoviště"

        if(insidePrice) {
            double price = Double.parseDouble(text);

            //Updating cheapest and the most expensive stadion
            if(price > maxStadionPrice) {
                maxStadionPrice = price;
                maxStadionId = currentStadionId;
            }
            if(price < minStadionPrice) {
                minStadionPrice = price;
                minStadionId = currentStadionId;
            }
        }

        if(insideMotto) {
            currentMotto.setText(text);

            //Updating oldest and newest motto
            if(oldestMotto == null || currentMotto.isLessThan(oldestMotto)) {
                if(oldestMotto == null) 
                    oldestMotto = new Motto();
 
                oldestMotto = (Motto) deepClone(currentMotto);
            }
 
            if(newestMotto == null || currentMotto.isGreaterThan(newestMotto)) {
                if(newestMotto == null) 
                    newestMotto = new Motto();

                newestMotto = (Motto)deepClone(currentMotto);
            }
 
        }

        if(insideFirstName) 
            current.setFirstName(text); //we have constructed person with only a first name, we need to wait for last name
            
        if(insideLastName) {
            current.setLastName(text);

            //We have constructed Person object completely, time to check,
            //whether it is well sorted with preceding Person
            if(previous == null || previous.isBefore(current)) {
                previous = (Person)deepClone(current);
            }
            else {
                sortedByLastNameThenFirstName = false;
            }
        }
 
    }
    
    /**
     * Namespace declaration beginning handler
     * @param prefix Prefix of the namespace
     * @param uri URI of the namespace
     * @throws SAXException
     */
    @Override
    public void startPrefixMapping(
        String prefix, String uri
    ) throws SAXException {
        // ...
    }
    
    /**
     * Namespace declaration end handler
     * @param prefix Prefix of the namespace
     * @throws SAXException
     */
    @Override
    public void endPrefixMapping(String prefix) throws SAXException {
        // ...
    }

    /**
     * Ignorable whitespace characters handler
     * @param chars Array with character data
     * @param start Index of the start position in the array
     * @param length Number of characters to read from the array
     * @throws SAXException
     */
    @Override
    public void ignorableWhitespace(
        char[] chars, int start, int length
    ) throws SAXException {
        // ...
    }
    
    /**
     * Processing instruction handler
     * @param target Processing instruction target
     * @param data Processing instruction data
     * @throws SAXException
     */
    @Override
    public void processingInstruction(
        String target, String data
    ) throws SAXException {
        // ...
    }
    
    /**
     * Unprocessed entity handler
     * @param name Name of the skipped entity
     * @throws SAXException
     */
    @Override
    public void skippedEntity(String name) throws SAXException {
        // ...
    }
    
    private <T extends Number> Double avg(ArrayList<T> list) {
        double sum = 0;
        for(int i = 0; i < list.size(); ++i) 
            sum += Double.parseDouble(list.get(i).toString());
  
        return sum / list.size();
    }

    boolean tryParseInt(String value) {  
        try {  
            Integer.parseInt(value);  
            return true;  
         } catch (NumberFormatException e) {  
            return false;  
         }  
   }

   public static Object deepClone(Object object) {
    try {
        ByteArrayOutputStream baOs = new ByteArrayOutputStream();
        ObjectOutputStream oOs = new ObjectOutputStream(baOs);
        oOs.writeObject(object);
        ByteArrayInputStream baIs = new ByteArrayInputStream(baOs.toByteArray());
        ObjectInputStream oIs = new ObjectInputStream(baIs);
        return oIs.readObject();
    }
    catch (Exception e) {
        e.printStackTrace();
        return null;
    }
}
    
}

class Motto implements Serializable {
    private static final long serialVersionUID = 3066463939679975289L;

    private Integer year;
    private String text;

    public void setYear(Integer newYear) { year = newYear; }
    public Integer getYear() { return year; }

    public void setText(String newText) { text = newText; }
    public String getText() { return text; }

    public boolean isLessThan(Motto rhs) { return year < rhs.getYear(); }
    public boolean isGreaterThan(Motto rhs) { return year > rhs.getYear(); }
}

class Person implements Serializable {
    private static final long serialVersionUID = -8086816949573385074L;

    private String firstName;
    private String lastName;

    public void setFirstName(String newFirstName) { firstName = newFirstName; }
    public String getFirstName() { return firstName; }

    public void setLastName(String newLastName) { lastName = newLastName; }
    public String getLastName() { return lastName; }

    private String capitalizeFirst(String txt) { return txt.substring(0, 1).toUpperCase() + txt.substring(1); }

    public boolean isBefore(Person rhs) {
        Integer c1 = capitalizeFirst(lastName).compareTo(capitalizeFirst(rhs.getLastName()));
        Integer c2 = capitalizeFirst(firstName).compareTo(capitalizeFirst(rhs.getFirstName()));
        if(c1 == 0) 
            return c2 < 0;
       
        return c1 < 0;
    }
}
