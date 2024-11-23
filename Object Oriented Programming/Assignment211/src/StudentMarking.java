import java.util.Scanner;
// DO NOT import anything else

/**
 * This class forms Java Assignment 1, 2021
 */
public class StudentMarking {

    /**
     * The message that the main menu must display --- DO NOT CHANGE THIS
     */
    public final String MENU_TEMPLATE =
            "%nWelcome to the Student System. Please enter an option 0 to 3%n"
                    + "0. Exit%n"
                    + "1. Generate a student ID%n"
                    + "2. Capture marks for students%n"
                    + "3. List student IDs and average mark%n";
    /**
     * DO NOT CHANGE THIS
     */
    public final String NOT_FOUND_TEMPLATE =
            "No student id of %s exists";


    /* Do NOT change the two templates ABOVE this comment.
      DO CHANGE the templates BELOW.
   */

    // TODO (All questions) - Complete these templates which will be used throughout the program
    public final String ENTER_MARK_TEMPLATE = "Please enter mark %d for student %s%n";
    public final String STUDENT_ID_TEMPLATE = "You entered a Givenname of %s and a Familyname of %s%n" +
            "Your studID is %s%n";
    public final String NAME_RESPONSE_TEMPLATE =
            "Please enter your Givenname and Familyname (Enter 0 to return to main menu)%n";
    public final String LOW_HIGH_TEMPLATE = "Student %s has a lowest mark of %d%n" +
            "A highest mark of %d%n";
    public final String AVG_MARKS_TEMPLATE = "Student ***%s*** has an average of  %.2f%n";
    public final String COLUMN_1_TEMPLATE = "   *%n" +
            "   *%n" +
            "   *           *%n" +
            "   *           *%n" +
            "   *           *%n" +
            "   *           *%n" +
            "   *           *%n";
    public final String COLUMN_2_TEMPLATE = "Highest     Lowest%n";
    public final String CHART_KEY_TEMPLATE = "   %d         %d%n";
    public final String REPORT_PER_STUD_TEMPLATE = "| Student ID   %d is %sl | Average is  %.2f |%n";

    /**
     * Creates a student ID based on user input
     *
     * @param sc Scanner reading {@link System#in} re-used from {@link StudentMarking#main(String[])}
     * @return a studentID according to the pattern specified in {@link StudentMarking#STUDENT_ID_TEMPLATE}
     */
    public String generateStudId(Scanner sc) {
        // TODO (3.4) - Complete the generateStudId method which will allow a user to generate a student id
        String studId = "generateStudId is incomplete"; // TODO Don't have unnecessary initialisations
        char givenNameInitial;
        char surNameInitial;
        String Family_name_length;
        char givenNameMiddleLetter;
        char surnameMiddleLetter;
        System.out.printf(NAME_RESPONSE_TEMPLATE);
        String givenName = sc.next();
        if (givenName.equals("0")) {
            return studId;
        }
        givenNameInitial = givenName.toUpperCase().charAt(0);
        givenNameMiddleLetter = givenName.toLowerCase().charAt(givenName.length() / 2);

        String Familyname = sc.next();

        surNameInitial = Familyname.toUpperCase().charAt(0);
        surnameMiddleLetter = Familyname.toLowerCase().charAt(Familyname.length() / 2);
        Family_name_length = String.format("%02d", Familyname.length());
        studId = "" + givenNameInitial + surNameInitial + Family_name_length + givenNameMiddleLetter +
                surnameMiddleLetter;
        System.out.printf(STUDENT_ID_TEMPLATE, givenName, Familyname, studId);
        return studId;
    }

    /**
     * Reads three marks (restricted to a floor and ceiling) for a student and returns their mean
     *
     * @param sc     Scanner reading {@link System#in} re-used from {@link StudentMarking#main(String[])}
     * @param studId a well-formed ID created by {@link StudentMarking#generateStudId(Scanner)}
     * @return the mean of the three marks entered for the student
     */
    public double captureMarks(Scanner sc, String studId) {
        // TODO (3.5) - Complete the captureMarks method which will allow a user to input three mark for a chosen student
        // DO NOT change MAX_MARK and MIN_MARK
        final int MAX_MARK = 100;
        final int MIN_MARK = 0;
        final String Range_Wrong_Message = "The range you entered is incorrect, please re-enter%n";
        int[] mark = new int[3];
        int high = MIN_MARK;
        int low = MAX_MARK;
        double avg; // TODO Don't have unnecessary initialisations
        String answer;
        for (int i = 0; i < 3; i++) {
            while (true) {
                System.out.printf(ENTER_MARK_TEMPLATE, i + 1, studId);
                mark[i] = sc.nextInt();
                //to make sure the mark range from 0 to 100
                if (mark[i] <= MAX_MARK && mark[i] >= MIN_MARK) {
                    break;
                } else {
                    System.out.printf(Range_Wrong_Message);
                }
            }
        }
        for (int i = 0; i < 3; i++) {
            if (mark[i] > high)
                high = mark[i];
            if (mark[i] < low)
                low = mark[i];
        }

        avg = (double) (mark[0] + mark[1] + mark[2]) / 3;
        System.out.printf(LOW_HIGH_TEMPLATE, studId, low, high);
        System.out.printf(AVG_MARKS_TEMPLATE, studId, avg);
        System.out.printf("Would you like to print a bar chart? [y/n]%n");
        answer = sc.next();
        if (answer.equals("Y") || answer.equals("y"))
            printBarChart(studId, high, low);
        return avg;
    }

    /**
     * outputs a simple character-based vertical bar chart with 2 columns
     *
     * @param studId a well-formed ID created by {@link StudentMarking#generateStudId(Scanner)}
     * @param high   a student's highest mark
     * @param low    a student's lowest mark
     */
    public void printBarChart(String studId, int high, int low) {
        // TODO (3.6) - Complete the printBarChart method which will print a bar chart of the highest and lowest results of a student
        System.out.printf("Student id statistics: %s%n", studId);
        System.out.printf(COLUMN_1_TEMPLATE);
        System.out.printf(COLUMN_2_TEMPLATE);
        System.out.printf(CHART_KEY_TEMPLATE, high, low);
    }

    /**
     * Prints a specially formatted report, one line per student
     *
     * @param studList student IDs originally generated by {@link StudentMarking#generateStudId(Scanner)}
     * @param count    the total number of students in the system
     * @param avgArray mean (average) marks
     */
    public void reportPerStud(String[] studList,
                              int count,
                              double[] avgArray) {
        // TODO (3.7) - Complete the reportPerStud method which will print all student IDs and average marks
        for (int i = 0; i < count; i++) {
            System.out.printf(REPORT_PER_STUD_TEMPLATE, i + 1, studList[i], avgArray[i]);
        }

    }

    /**
     * The main menu
     */
    public void displayMenu() {
        // TODO (3.2) - Complete the displayMenu method to use the appropriate template with printf
        System.out.printf(MENU_TEMPLATE);
    }

    /**
     * The controlling logic of the program. Creates and re-uses a {@link Scanner} that reads from {@link System#in}.
     *
     * @param args Command-line parameters (ignored)
     */
    public static void main(String[] args) {
        // TODO (3.3) - Complete the main method to give the program its core

        // DO NOT change sc, sm, EXIT_CODE, and MAX_STUDENTS
        Scanner sc = new Scanner(System.in);
        StudentMarking sm = new StudentMarking();
        final int EXIT_CODE = 0;
        final int MAX_STUDENTS = 5;
        String studId;
        int flag;
        int Option;
        int count = 0;
        int IdNumber = 0;
        double Avg = 0;

        // TODO Initialise these
        String[] keepStudId = new String[MAX_STUDENTS];
        double[] avgArray = new double[MAX_STUDENTS];

        // TODO Build a loop around displaying the menu and reading then processing input
        while (true) {
            sm.displayMenu();
            Option = sc.nextInt();
            if (Option == EXIT_CODE)
                break;
            else if (Option == 1) {
                studId = sm.generateStudId(sc);
                //to make sure generate a new student ID
                if (!studId.equals("generateStudId is incomplete")) {
                    keepStudId[IdNumber++] = studId;
                    count++;
                }
            } else if (Option == 2) {
                System.out.printf(
                        "Please enter the studId to capture their marks (Enter 0 to return to main menu)%n");
                studId = sc.next();
                if (studId.equals(String.valueOf(EXIT_CODE))) continue;

                flag = 0;
                for (int i = 0; i < count; i++) {
                    if (keepStudId[i].equals(studId)) {
                        Avg = sm.captureMarks(sc, studId);
                        flag = 1;
                    }
                }
                if (flag == 0)
                    System.out.printf(sm.NOT_FOUND_TEMPLATE, studId);
                //to make sure the position of student ID and mean score are same in the array
                for (int i = 0; i < count; i++) {
                    if (keepStudId[i].equals(studId))
                        avgArray[i] = Avg;
                }
            } else if (Option == 3) {
                sm.reportPerStud(keepStudId, count, avgArray);
            } else {
                // Handle invalid main menu input
                System.out.printf(
                        "You have entered an invalid option. Enter 0, 1, 2 or 3%n");// Skeleton: keep, unchanged
            }
        }
        System.out.printf("Goodbye%n");
    }
}

/*
    TODO Before you submit:
         1. ensure your code compiles
         2. ensure your code does not print anything it is not supposed to
         3. ensure your code has not changed any of the class or method signatures from the skeleton code
         4. check the Problems tab for the specific types of problems listed in the assignment document
         5. reformat your code: Code > Reformat Code
         6. ensure your code still compiles (yes, again)
 */