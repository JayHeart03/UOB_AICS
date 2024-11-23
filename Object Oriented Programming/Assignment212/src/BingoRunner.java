public class BingoRunner {
    public static void main(String[] args) {
        new BingoController().run();
    /* TODO
          create and execute a new BingoController that starts the game
          invoke run()
          include the Thank you for playing once the game exits (GOODBYEMESSAGE)
     */
        System.out.printf(Toolkit.GOODBYEMESSAGE + "\n");
    }
}
