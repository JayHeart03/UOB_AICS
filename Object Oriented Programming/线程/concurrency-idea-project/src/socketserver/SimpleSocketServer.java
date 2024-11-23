package socketserver;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.PrintWriter;
import java.net.ServerSocket;
import java.net.Socket;

public class SimpleSocketServer {
    private ServerSocket serverSocket;
    private int port;
    private boolean running = false;

    public SimpleSocketServer(int port) {
        this.port = port;
    }

    public void startServer() {
        try {
            serverSocket = new ServerSocket(port);
            this.handleRequest();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    public void handleRequest() {
        while (true) {
            try {
                System.out.println("Listening for a connection");

                // Call accept() to receive the next connection
                Socket socket = serverSocket.accept();

                // Pass the socket to the RequestHandler thread for processing
                System.out.println("Received a connection");

                // Get input and output streams
                BufferedReader in = new BufferedReader(new InputStreamReader(socket.getInputStream()));
                PrintWriter out = new PrintWriter(socket.getOutputStream());

                // Write out our header to the client
                out.println("Echo Server 1.0");
                out.flush();

                // Echo lines back to the client until the client closes the connection or we receive an empty line
                String line = in.readLine();
                while (line != null && line.length() > 0) {
                    out.println("Echo: " + line);
                    out.flush();
                    line = in.readLine();
                }

                // Close our connection
                in.close();
                out.close();
                socket.close();

                System.out.println("Connection closed");
            } catch (IOException e) {
                e.printStackTrace();
            }
        }
    }

    public static void main(String[] args) {
        if (args.length == 0) {
            System.out.println("Usage: SimpleSocketServer <port>");
            System.exit(0);
        }
        int port = Integer.parseInt(args[0]);

        System.out.println("Start server on port: " + port);
        System.out.println("exit using ctrl+C");

        SimpleSocketServer server = new SimpleSocketServer(port);
        server.startServer();
    }
}
