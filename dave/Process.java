package dave;

import java.io.IOException;
import java.net.InetAddress;
import java.net.InetSocketAddress;
import java.nio.channels.SelectionKey;
import java.nio.channels.Selector;
import java.nio.channels.ServerSocketChannel;
import java.nio.channels.SocketChannel;
import java.nio.channels.spi.SelectorProvider;
import java.util.*;

/**
 */
public class Process
{

    public class ChangeRequest
    {
        public static final int REGISTER = 1;
        public static final int CHANGEOPS = 2;

        public SocketChannel socket;
        public int type;
        public int ops;

        public ChangeRequest (SocketChannel socket, int type, int ops)
        {
            this.socket = socket;
            this.type = type;
            this.ops = ops;
        }
    }

    // The host:port combination to listen on
    protected InetAddress hostAddress;
    protected int port;

    // A list of ChangeRequest instances
    protected final List<ChangeRequest> changeRequests = new java.util.LinkedList<ChangeRequest>();

    public Process(int port, InetAddress hostAddress) {
        this.port = port;
        this.hostAddress = hostAddress;
    }

    protected Selector initSelector() throws IOException
    {
        // Create a new selector
        Selector socketSelector = SelectorProvider.provider().openSelector();

        return socketSelector;
    }


    private SocketChannel initiateConnection() throws IOException
    {
        // Create a non-blocking socket channel
        SocketChannel socketChannel = SocketChannel.open();
        socketChannel.configureBlocking(false);

        // Kick off connection establishment
        socketChannel.connect(new InetSocketAddress(this.hostAddress, this.port));

        // Queue a channel registration since the caller is not the
        // selecting thread. As part of the registration we'll register
        // an interest in connection events. These are raised when a channel
        // is ready to complete connection establishment.
        synchronized(this.changeRequests) {
            this.changeRequests.add(new ChangeRequest(socketChannel, ChangeRequest.REGISTER, SelectionKey.OP_CONNECT));
        }

        return socketChannel;
    }
}
