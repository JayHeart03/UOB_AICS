import java.util.HashMap;
import java.util.Map;
import java.util.Scanner;


public class Mapping {

    public static final int INITIAL_LOCATION = 95;

    /**
     * TODO
     * create a static LocationMap object
     */
    static LocationMap l = new LocationMap();

    /**
     * TODO
     * create a vocabulary HashMap to store all directions a user can go
     */
    HashMap<String, String> vocabulary = new HashMap<String, String>();

    /**
     * TODO
     * create a FileLogger object
     */
    FileLogger f = new FileLogger();


    /**
     * TODO
     * create a ConsoleLogger object
     */
    ConsoleLogger c = new ConsoleLogger();

    public Mapping() {
        //vocabulary.put("QUIT", "Q"); //example
        /** TODO
         * complete the vocabulary HashMap <Key, Value> with all directions.
         * use the directions.txt file and crosscheck with the ExpectedInput and ExpectedOutput files to find the keys and the values
         */
        vocabulary.put("QUIT", "Q");
        vocabulary.put("WEST", "W");
        vocabulary.put("EAST", "E");
        vocabulary.put("NORTH", "N");
        vocabulary.put("SOUTH", "S");
        vocabulary.put("UP", "U");
        vocabulary.put("DOWN", "D");
        vocabulary.put("SOUTHWEST", "SW");
        vocabulary.put("SOUTHEAST", "SE");
        vocabulary.put("NORTHEAST", "NE");
        vocabulary.put("NORTHWEST", "NW");
    }

    public void mapping() {

        /** TODO
         * create a Scanner object
         */
        Scanner sc = new Scanner(System.in);
        /**
         * initialise a location variable with the INITIAL_LOCATION
         */
        int location = INITIAL_LOCATION;


        while (true) {
            /** TODO
             * get the location and print its description to both console and file
             * use the FileLogger and ConsoleLogger objects
             */
            Location currentLocation = l.get(location);
            f.log(currentLocation.getDescription());
            c.log(currentLocation.getDescription());

            /** TODO
             * verify if the location is exit
             */
            if (location == 0) {
                break;
            }

            /** TODO
             * get a map of the exits for the location
             */
            Map<String, Integer> exits = currentLocation.getExits();

            /** TODO
             * print the available exits (to both console and file)
             * crosscheck with the ExpectedOutput files
             * Hint: you can use a StringBuilder to append the exits
             */
            StringBuilder sb = new StringBuilder();
            for (Object k : exits.keySet()) {
                sb.append(k + ", ");
                String.valueOf(sb);
            }
            f.log("Available exits are " + sb);
            c.log("Available exits are " + sb);


            /** TODO
             * input a direction
             * ensure that the input is converted to uppercase
             */
            String userInput = sc.nextLine().toUpperCase();

            /** TODO
             * are we dealing with a letter / word for the direction to go to?
             * available inputs are: a letter(the HashMap value), a word (the HashMap key), a string of words that contains the key
             * crosscheck with the ExpectedInput and ExpectedOutput files for examples of inputs
             * if the input contains multiple words, extract each word
             * find the direction to go to using the vocabulary mapping
             * if multiple viable directions are specified in the input, choose the last one
             */
            String l = userInput;
            String[] w = userInput.split(" ");
            String dir = "";
            for (String str : w) {
                //if equals letter
                if (userInput.equals("N") ||
                        userInput.equals("E") ||
                        userInput.equals("S") ||
                        userInput.equals("W") ||
                        userInput.equals("Q") ||
                        userInput.equals("U") ||
                        userInput.equals("D") ||
                        userInput.equals("SW") ||
                        userInput.equals("SE") ||
                        userInput.equals("NE") ||
                        userInput.equals("NW")) {
                    dir = userInput;
                } else {
                    //word and string
                    String retFremMap = (String) vocabulary.get(str);
                    if (retFremMap != null) {
                        dir = retFremMap;
                    }
                }
            }


            /** TODO
             * if user can go in that direction, then set the location to that direction
             * otherwise print an error message (to both console and file)
             * check the ExpectedOutput files
             */
            Integer existLocationNumber = (Integer) exits.get(dir);
            if (existLocationNumber == null) {
                f.log("You cannot go in that direction");
                c.log("You cannot go in that direction");
            } else {
                location = existLocationNumber;
            }

        }
    }

    public static void main(String[] args) {
        /**TODO
         * run the program from here
         * create a Mapping object
         * start the game
         */
        new Mapping().mapping();
    }

}
