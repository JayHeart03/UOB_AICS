import java.io.*;
import java.util.*;

//class that behaves like a map
public class LocationMap implements Map<Integer, Location> {

    private static final String LOCATIONS_FILE_NAME = "locations.txt";
    private static final String DIRECTIONS_FILE_NAME = "directions.txt";

    /**
     * TODO
     * create a static locations HashMap
     */
    static HashMap<Integer, Location> lcs = new HashMap<Integer, Location>();

    static {
        /** TODO
         * create a FileLogger object
         */
        FileLogger fl = new FileLogger();
        /** TODO
         * create a ConsoleLogger object
         */
        ConsoleLogger cl = new ConsoleLogger();

        /** TODO
         * Read from LOCATIONS_FILE_NAME so that a user can navigate from one location to another
         * use try-with-resources/catch block for the FileReader
         * extract the location and the description on each line
         * print all locations and descriptions to both console and file
         * check the ExpectedOutput files
         * put each location in the locations HashMap using temporary empty hashmaps for exits
         */
        try {
            File locations = new File(LOCATIONS_FILE_NAME);
            locations.createNewFile();
            FileReader fileReader = new FileReader(locations);
            BufferedReader bufferedReader = new BufferedReader(fileReader);
            String Line = "";
            fl.log("Available locations:");
            cl.log("Available locations:");
            while ((Line = bufferedReader.readLine()) != null) {
                int i = Integer.valueOf(Line.substring(0, Line.indexOf(",")));
                String lo = Line.substring(Line.indexOf(",") + 1);
                Location location = new Location(i, lo, null);
                fl.log(location.getLocationId() + ": " + location.getDescription());
                cl.log(location.getLocationId() + ": " + location.getDescription());
                lcs.put(i, location);
            }
            fileReader.close();
        } catch (FileNotFoundException e) {
            e.printStackTrace();
        } catch (IOException e) {
            e.printStackTrace();
        }


        /**TODO
         * Read from DIRECTIONS_FILE_NAME so that a user can move from A to B, i.e. current location to next location
         * use try-with-resources/catch block for the FileReader
         * extract the 3 elements  on each line: location, direction, destination
         * print all locations, directions and destinations to both console and file
         * check the ExpectedOutput files
         * for each location, create a new location object and add its exit
         */
        try {
            File directions = new File(DIRECTIONS_FILE_NAME);
            directions.createNewFile();
            FileReader fileReader = new FileReader(directions);
            BufferedReader bufferedReader = new BufferedReader(fileReader);
            String Line = "";
            fl.log("Available directions:");
            cl.log("Available directions:");
            while ((Line = bufferedReader.readLine()) != null) {
                int j = Integer.valueOf(Line.substring(0, Line.indexOf(",")));//first ,
                String di = Line.substring(Line.indexOf(",") + 1);//after first , string
                int k = Integer.valueOf(di.indexOf(","));//second ,
                String fi = di.substring(0, di.indexOf(","));//mid
                int z = Integer.valueOf(di.substring(di.indexOf(",") + 1));
                lcs.get(j).addExit(fi, z);
                fl.log(j + ": " + fi + ": " + z);
                cl.log(j + ": " + fi + ": " + z);
            }
            fileReader.close();
        } catch (FileNotFoundException e) {
            e.printStackTrace();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    /**
     * TODO
     * implement all methods for Map
     *
     * @return
     */
    @Override
    public int size() {
        //TODO
        return lcs.size();
    }

    @Override
    public boolean isEmpty() {
        //TODO
        return lcs.isEmpty();
    }

    @Override
    public boolean containsKey(Object key) {
        //TODO
        return lcs.containsKey(key);
    }

    @Override
    public boolean containsValue(Object value) {
        //TODO
        return lcs.containsKey(value);
    }

    @Override
    public Location get(Object key) {
        //TODO
        return lcs.get(key);
    }

    @Override
    public Location put(Integer key, Location value) {
        //TODO
        return lcs.put(key, value);
    }

    @Override
    public Location remove(Object key) {
        //TODO
        return lcs.remove(key);
    }

    @Override
    public void putAll(Map<? extends Integer, ? extends Location> m) {
        //TODO
        lcs.putAll(m);
    }

    @Override
    public void clear() {
        lcs.clear();
        //TODO
    }

    @Override
    public Set<Integer> keySet() {
        //TODO
        return lcs.keySet();
    }

    @Override
    public Collection<Location> values() {
        //TODO
        return lcs.values();
    }

    @Override
    public Set<Entry<Integer, Location>> entrySet() {
        //TODO
        return lcs.entrySet();
    }
}
