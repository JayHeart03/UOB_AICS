import java.io.*;
import java.util.*;

//class that behaves like a map
public class LocationMap implements Map<Integer, Location> {

    private static final String LOCATIONS_FILE_NAME =  "locations.txt";
    private static final String DIRECTIONS_FILE_NAME =  "directions.txt";

    /** TODO
     * create a static locations HashMap
     */
    static {
        /** TODO
         * create a FileLogger object
         */

        /** TODO
         * create a ConsoleLogger object
         */

        /** TODO
         * Read from LOCATIONS_FILE_NAME so that a user can navigate from one location to another
         * use try-with-resources/catch block for the FileReader
         * extract the location and the description on each line
         * print all locations and descriptions to both console and file
         * check the ExpectedOutput files
         * put each location in the locations HashMap using temporary empty hashmaps for exits
         */

        /**TODO
         * Read from DIRECTIONS_FILE_NAME so that a user can move from A to B, i.e. current location to next location
         * use try-with-resources/catch block for the FileReader
         * extract the 3 elements  on each line: location, direction, destination
         * print all locations, directions and destinations to both console and file
         * check the ExpectedOutput files
         * for each location, create a new location object and add its exit
         */

    }

    /**TODO
     * implement all methods for Map
     * @return
     */
    @Override
    public int size() {
        //TODO
        return 0;
    }

    @Override
    public boolean isEmpty() {
        //TODO
        return true;
    }

    @Override
    public boolean containsKey(Object key) {
        //TODO
        return true;
    }

    @Override
    public boolean containsValue(Object value) {
        //TODO
        return true;
    }

    @Override
    public Location get(Object key) {
        //TODO
        return null;
    }

    @Override
    public Location put(Integer key, Location value) {
        //TODO
        return null;
    }

    @Override
    public Location remove(Object key) {
        //TODO
        return null;
    }

    @Override
    public void putAll(Map<? extends Integer, ? extends Location> m) {
        //TODO
    }

    @Override
    public void clear() {
        //TODO
    }

    @Override
    public Set<Integer> keySet() {
        //TODO
        return null;
    }

    @Override
    public Collection<Location> values() {
        //TODO
        return null;
    }

    @Override
    public Set<Entry<Integer, Location>> entrySet() {
        //TODO
        return null;
    }
}
