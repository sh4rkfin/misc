package dave;

import java.io.IOException;
import java.net.InetAddress;
import java.net.InetSocketAddress;
import java.net.Socket;
import java.nio.ByteBuffer;
import java.nio.channels.SelectionKey;
import java.nio.channels.Selector;
import java.nio.channels.ServerSocketChannel;
import java.nio.channels.SocketChannel;
import java.nio.channels.spi.SelectorProvider;
import java.util.*;
import java.util.LinkedList;

/**
*/
public class Server extends Process implements Runnable
{

    // The selector we'll be monitoring
    private Selector selector;

    // The channel on which we'll accept connections
    private ServerSocketChannel serverChannel;

    // The buffer into which we'll read data when it's available
    private ByteBuffer readBuffer = ByteBuffer.allocate(8192);

    // Maps a SocketChannel to a list of ByteBuffer instances
    protected final Map<SocketChannel,List<ByteBuffer>> pendingData
            = new HashMap<SocketChannel,List<ByteBuffer>>();


    public Server (InetAddress hostAddress, int port) throws IOException
    {
        super(port, hostAddress);
        this.selector = this.initSelector();
    }

    @Override
    protected Selector initSelector() throws IOException
    {
        Selector result = super.initSelector();

        // Create a new non-blocking server socket channel
        this.serverChannel = ServerSocketChannel.open();
        serverChannel.configureBlocking(false);

        // Bind the server socket to the specified address and port
        InetSocketAddress isa = new InetSocketAddress(this.hostAddress, this.port);
        serverChannel.socket().bind(isa);

        // Register the server socket channel, indicating an interest in
        // accepting new connections
        serverChannel.register(result, SelectionKey.OP_ACCEPT);
        return result;
    }

    private void accept (SelectionKey key)
    throws IOException
    {
        // For an accept to be pending the channel must be a server socket channel.
        ServerSocketChannel serverSocketChannel = (ServerSocketChannel)key.channel();

        // Accept the connection and make it non-blocking
        SocketChannel socketChannel = serverSocketChannel.accept();
        Socket socket = socketChannel.socket();
        socketChannel.configureBlocking(false);

        // Register the new SocketChannel with our Selector, indicating
        // we'd like to be notified when there's data waiting to be read
        socketChannel.register(this.selector, SelectionKey.OP_READ);
    }

    private void read (SelectionKey key)
    throws IOException
    {
        SocketChannel socketChannel = (SocketChannel)key.channel();

        // Clear out our read buffer so it's ready for new data
        readBuffer.clear();

        // Attempt to read off the channel
        int numRead;
        try {
            numRead = socketChannel.read(this.readBuffer);
        } catch (IOException e) {
            // The remote forcibly closed the connection, cancel
            // the selection key and close the channel.
            key.cancel();
            socketChannel.close();
            return;
        }

        if (numRead == -1) {
            // Remote entity shut the socket down cleanly. Do the
            // same from our end and cancel the channel.
            key.channel().close();
            key.cancel();
            return;
        }

        byte[] bytes = this.readBuffer.array();
        System.out.println("read: " + new String(bytes));

        // Hand the data off to our worker thread
        //worker.processData(this, socketChannel, this.readBuffer.array(), numRead);
    }

    public void send (SocketChannel socket, byte[] data)
    {
        synchronized (changeRequests) {
            // Indicate we want the interest ops set changed
            changeRequests.add(new ChangeRequest(socket, ChangeRequest.CHANGEOPS, SelectionKey.OP_WRITE));

            // And queue the data we want written
            synchronized (pendingData) {
                List<ByteBuffer> queue = pendingData.get(socket);
                if (queue == null) {
                    queue = new ArrayList<ByteBuffer>();
                    pendingData.put(socket, queue);
                }
                queue.add(ByteBuffer.wrap(data));
            }
        }

        // Finally, wake up our selecting thread so it can make the required changes
        selector.wakeup();
    }

    private void write (SelectionKey key)
    throws IOException
    {
        SocketChannel socketChannel = (SocketChannel)key.channel();

        synchronized (pendingData)
        {
            List<ByteBuffer> queue = pendingData.get(socketChannel);

            // Write until there's not more data ...
            while (!queue.isEmpty()) {
                ByteBuffer buf = queue.get(0);
                socketChannel.write(buf);
                if (buf.remaining() > 0) {
                    // ... or the socket's buffer fills up
                    break;
                }
                queue.remove(0);
            }

            if (queue.isEmpty()) {
                // We wrote away all data, so we're no longer interested
                // in writing on this socket. Switch back to waiting for
                // data.
                key.interestOps(SelectionKey.OP_READ);
            }
        }
    }

    public void run()
    {
        while (true) {
            try {
                // Process any pending changes
                synchronized(changeRequests) {
                    Iterator<ChangeRequest> changes = changeRequests.iterator();
                    while (changes.hasNext()) {
                        ChangeRequest change = changes.next();
                        switch(change.type) {
                            case ChangeRequest.CHANGEOPS:
                                SelectionKey key = change.socket.keyFor(selector);
                                key.interestOps(change.ops);
                        }
                    }
                    this.changeRequests.clear();
                }
                // Wait for an event one of the registered channels
                selector.select();

                // Iterate over the set of keys for which events are available
                Iterator selectedKeys = selector.selectedKeys().iterator();
                while (selectedKeys.hasNext())
                {
                    SelectionKey key = (SelectionKey)selectedKeys.next();
                    selectedKeys.remove();

                    if (!key.isValid()) {
                        continue;
                    }

                    // Check what event is available and deal with it
                    if (key.isAcceptable()) {
                        this.accept(key);
                    }
                    else if (key.isReadable()) {
                        this.read(key);
                    }
                    else if (key.isWritable()) {
                        this.write(key);
                    }
                }
            }
            catch (Exception e) {
                e.printStackTrace();
            }
        }
    }

    public static void main(String[] args) {
        try {
            new Thread(new Server(null, 9090)).start();
        }
        catch (IOException e) {
            e.printStackTrace();
        }
    }
}
