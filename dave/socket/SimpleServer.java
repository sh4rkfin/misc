package dave.socket;

import java.io.*;
import java.net.ServerSocket;
import java.net.Socket;

/**
    Running instructions:
    cd to ~/IdeaProjects/misc
    java -cp out/production/misc/ dave.socket.SimpleServer
*/
public class SimpleServer
{
    public static void close (Socket socket)
    {
        try {
            if (socket != null) {
                socket.close();
            }
        }
        catch (IOException ex)
        {}
    }

    public static void close (Reader reader)
    {
        try {
            if (reader != null) {
                reader.close();
            }
        }
        catch (IOException ex)
        {}
    }

    public static void close (Writer writer)
    {
        try {
            if (writer != null) {
                writer.close();
            }
        }
        catch (IOException ex)
        {}
    }


    public static class Worker implements Runnable
    {
        Socket _socket;

        public Worker (Socket socket)
        {
            this._socket = socket;
        }

        public void run ()
        {
            BufferedReader r = null;
            PrintWriter w = null;
            try {
                r = new BufferedReader(new InputStreamReader(_socket.getInputStream()));
                w = new PrintWriter(new BufferedWriter(new OutputStreamWriter(_socket.getOutputStream())));
                String s;
                while ((s=r.readLine()) != null) {
                    System.out.println("Received: '" + s + "'");
                    w.println(s);
                    w.flush();
                }
            }
            catch (Exception ex)
            {
                throw new RuntimeException(ex);
            }
            finally {
                close(w);
                close(r);
                close(_socket);
            }
        }
    }


    /**
        Runs the server.
    */
    public static void main(String[] args) throws IOException
    {
        System.out.println("Starting server...");
        ServerSocket listener = new ServerSocket(9090);
        try {
            while (true) {
                System.out.println("Listening ...");
                Socket socket = listener.accept();
                Thread t = new Thread(new Worker(socket));
                t.start();
            }
        }
        finally {
            listener.close();
        }
    }
}
