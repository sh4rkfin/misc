package dave.socket;

import java.io.*;
import java.net.Socket;

/**
    Trivial client for the date server.
*/
public class SimpleClient
{

    /**
    */
    public static void main(String[] args) throws IOException
    {
        String server = "localhost";
        Socket s = new Socket(server, 9090);
        // autoflush is important, otherwise you hang waiting for feedback
        PrintWriter w = new PrintWriter(new BufferedWriter(new OutputStreamWriter(s.getOutputStream())), true);
        w.println("foo");
        w.println("bar");
        w.println(".");
        BufferedReader input = new BufferedReader(new InputStreamReader(s.getInputStream()));
        String answer;
        while ((answer = input.readLine()) != null) {
            System.out.println("answer: " + answer);
            if (answer.equals(".")) {
                break;
            }
        }
        w.close();
        input.close();
        s.close();
    }
}