import java.io.*;
import java.net.*;

public class ClientHandler implements Runnable {
    //declare variables
    //Constructor
    Socket clientSocket;
    int clientNo;
    Database database;

    public ClientHandler(Socket socket, int clientId, Database db) {
        clientSocket = socket;
        clientNo = clientId;
        database = db;
        //complete the constructor
    }

    public void run() {
        try {
            System.out.println("ClientHandler started...");
            BufferedReader inFromClient = new BufferedReader(new InputStreamReader(clientSocket.getInputStream()));
            PrintWriter outToClient = new PrintWriter(clientSocket.getOutputStream(), true);
            String clientMessage;
            int titlesNum;
            while (!(clientMessage = inFromClient.readLine()).equals("stop")) {
                System.out.println("Client sent the artist name " + clientMessage);
                titlesNum = database.getTitles(clientMessage);
                outToClient.println("Number of titles: " + titlesNum + " records found");
            }
            /*System.out.println("ClientHandler started...");
              Create I/O streams to read/write data, PrintWriter and BufferedReader
              Receive messages from the client and send replies, until the user types "stop"
                  System.out.println("Client sent the artist name " + clientMessage);
                  Request the number of titles from the db
                  Send reply to Client:
                  outToClient.println("Number of titles: " + titlesNum + " records found");

              System.out.println("Client " + clientId + " has disconnected");
              outToClient.println("Connection closed, Goodbye!");
              Close I/O streams and socket*/
            System.out.println("Client " + clientNo + " has disconnected");
            outToClient.println("Connection closed, Goodbye!");
            inFromClient.close();
            outToClient.close();
            clientSocket.close();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
