import java.io.File;
import java.io.FileWriter;
import java.io.IOException;

public class FileLogger implements Logger {
    private static final String FILE_LOGGER_NAME =  "StudentFileOutput.txt";

    static {

        /** TODO
         * create a new File object for FILE_LOGGER_NAME
         * if the file already exists, delete it first
         * use try/catch block
         */
    }

    @Override
    public void log (String message) {
        /** TODO
         * create a new FileWriter in append mode
         * write the message to file
         * check the ExpectedOutput files
         * use try-with-resources/catch block
         */
    }
}
