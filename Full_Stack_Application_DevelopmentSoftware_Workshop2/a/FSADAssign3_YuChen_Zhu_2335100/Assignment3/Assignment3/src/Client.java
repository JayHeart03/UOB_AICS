import java.io.*;
import java.net.*;

public class Client {

    public static void main(String args[]) throws IOException {
        String artistName, serverMessage;
        Socket clientSocket = new Socket(Credentials.HOST, Credentials.PORT);
        System.out.println("Client is running");
        PrintWriter outToServer = new PrintWriter(clientSocket.getOutputStream(), true);
        BufferedReader inFromServer = new BufferedReader(new InputStreamReader(clientSocket.getInputStream()));
        BufferedReader inFromUser = new BufferedReader(new InputStreamReader(System.in));
        while (true) {
            System.out.println("Enter the artist name:");
            artistName = inFromUser.readLine();
            System.out.println("You entered " + artistName);
            outToServer.println(artistName);
            serverMessage = inFromServer.readLine();
            System.out.println("FROM SERVER: " + serverMessage);
            if (artistName.equals("stop"))
                break;
        }
        inFromServer.close();
        inFromUser.close();
        outToServer.close();
        clientSocket.close();
        /*Open a connection to the server, create the client socket
          System.out.println("Client is running");
          Create I/O streams to read/write data, PrintWriter and BufferedReader
          Read messages continuously until the user types "stop"
             System.out.println("Enter the artist name:");
             System.out.println("You entered " + artistName);
             Send message to the server
             Receive response from the server
             System.out.println("FROM SERVER: " + serverMessage);
          Close I/O streams and socket*/
    }
}
