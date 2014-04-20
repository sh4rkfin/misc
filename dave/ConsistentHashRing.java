package dave;

import java.security.MessageDigest;
import java.util.*;

/**
 * User: dfinlay
 * Date: 3/8/13
 */
public class ConsistentHashRing
{

    public static class Server
    {
        public final long hash;
        public final String serverName;
        public final int replicaId;

        public Server (String serverName, int replicaId)
        {
            this.serverName = serverName;
            String nameToHash = replicaId != 0 ? serverName + "." + replicaId : serverName;
            this.hash = hash(nameToHash);
            this.replicaId = replicaId;
        }

        private Server (long hash)
        {
            this.hash = hash;
            this.serverName = null;
            this.replicaId = -1;
        }

        @Override
        public boolean equals (Object that)
        {
            return that instanceof Server && equals((Server)that);
        }

        public boolean equals (Server that)
        {
            return (serverName != null ? serverName.equals(that.serverName) : that.serverName == null) &&
                    hash == that.hash &&
                    replicaId == that.replicaId;
        }

        public int hashCode ()
        {
            return serverName.hashCode() + (int)hash + replicaId;
        }

        public String toString ()
        {
            return hash + " " + serverName + "(" + replicaId + ")";
        }

        public static final Comparator<Server> HashComparator = new Comparator<Server>()
        {
            public int compare(Server first, Server second) {
                int result = Util.sgn(first.hash, second.hash);
                if (result == 0) {
                    result = first.serverName != null
                            ? (second.serverName != null ? first.serverName.compareTo(second.serverName) : +1)
                            : (second.serverName != null ? -1 : 0);
                    if (result == 0) {
                        return Util.sgn(first.replicaId, second.replicaId);
                    }
                }
                return result;
            }
        };
    }

    private int _replicaCount;
    private Server[] _servers;
    private TreeSet<String> _serverNames;
    private int _count;

    public ConsistentHashRing (int replicaCount)
    {
        _servers = new Server[256];
        _serverNames = new TreeSet<String>();
        _replicaCount = replicaCount;
    }

    public Map<String,Double> computeCoverage ()
    {
        Map<String,Double> result = new HashMap();
        if (_count == 0) {
            return result;
        }
        if (_count == 1) {
            result.put(_servers[0].serverName, 1.0);
            return result;
        }
        double maxl = Long.MAX_VALUE;
        double minl = Long.MIN_VALUE;
        Server last = _servers[_count - 1];
        double lastHash = last.hash + minl - maxl;
        for (int i = 0; i < _count; i++) {
            Server server = _servers[i];
            Double value = (double)server.hash - lastHash;
            Double current = result.get(server.serverName);
            result.put(server.serverName, current != null ? current + value : value);
            last = server;
            lastHash = server.hash;
        }
        for (String server : result.keySet()) {
            result.put(server, result.get(server) / (maxl - minl));
        }
        return result;
    }

    public static String makeReplicaName (String serverName, int replicaId)
    {
        return serverName + "." + replicaId;
    }

    public void addServer (String serverName)
    {
        _serverNames.add(serverName);
        for (int i=0; i<=_replicaCount; ++i) {
            Server server = new Server(serverName, i);
            add(server);
        }
    }

    public SortedSet<String> getAllServerNames ()
    {
        return (SortedSet<String>)_serverNames.clone();
    }

    public String getServer (String resource)
    {
        long hash = hash(resource);
        Server res = new Server(hash);
        int val = Arrays.binarySearch(_servers, 0, _count, res, Server.HashComparator);
        int idx = -val - 1;
        if (idx < _count) {
            return _servers[idx].serverName;
        }
        return _servers[0].serverName;
    }

    private void checkSize (int size)
    {
        if (size > _servers.length) {
            Server[] newServers = new Server[_servers.length * 2];
            System.arraycopy(_servers, 0, newServers, 0, _count);
            _servers = newServers;
        }
    }

    private void add (Server server)
    {
        int val = Arrays.binarySearch(_servers, 0, _count, server, Server.HashComparator);
        int idx = -1;
        if (val >= 0) {
            idx = val;
            Server existing = _servers[val];
            if (server.equals(existing)) {
                return;
            }
        }
        else {
            idx = -val - 1;
        }
        checkSize(_count + 1);
        System.arraycopy(_servers, idx, _servers, idx + 1, _count - idx);
        _servers[idx] = server;
        _count++;
    }

    public static String toString (byte val)
    {
        char[] result = new char[8];
        for (int i=0; i<result.length; ++i) {
            result[7 - i] = (val & (1 << i)) > 0 ? '1' : '0';
        }
        return new String(result);
    }

    public static String toString (int val)
    {
        String result = "";
        for (int i=3; i>=0; --i) {
            byte b = (byte)(val >>> (i*8));
            result += toString(b);
            if (i > 0) {
                result += ".";
            }
        }
        return result;
    }

    public static String toString (long val)
    {
        String result = "";
        for (int i=7; i>=0; --i) {
            byte b = (byte)(val >>> (i*8));
            result += toString(b);
            if (i > 0) {
                result += ".";
            }
        }
        return result;
    }


    private static long hash (Object value)
    {
        try {
            MessageDigest md = MessageDigest.getInstance("MD5");
            byte[] bytes = md.digest(value.toString().getBytes("UTF-8"));
            //for (byte val : bytes) {
            //    System.out.print("." + toString(val));
            //}
            //System.out.println("");
            long result = 0;
            for (int i=0; i<16; ++i) {
                long val = (0x000000ffL & bytes[i]) << ((i % 8) * 8);
                result ^= val;
                //System.out.println("result: " + toString(result));
            }
            return result;
        }
        catch (Exception ex) {
            throw new RuntimeException(ex);
        }
    }

    public static ConsistentHashRing makeHashRing (int serverCount, int replicaCount)
    {
        ConsistentHashRing result = new ConsistentHashRing(replicaCount);
        Random rnd = new Random();
        TreeSet<Integer> mgrs = new TreeSet();
        while (mgrs.size() < serverCount) {
            mgrs.add(rnd.nextInt(100));
        }
        for (Integer mgr : mgrs) {
            result.addServer("Manager-" + mgr);
        }
        return result;
    }

    public static Map<String,Integer> computeCountsForResources (ConsistentHashRing ring, int resourceCount)
    {
        Map<String,Integer> results = new HashMap<String,Integer>();
        for (int i=0; i<resourceCount; ++i) {
            String server = ring.getServer("CommunityWorkQueue-"+i);
            Integer count = results.get(server);
            results.put(server, (count != null) ? count + 1 : 1);
        }
        return results;
    }

    public static int testOneMgr (int serverCount, int replicaCount)
    {
        ConsistentHashRing ring = makeHashRing(serverCount, replicaCount);
        Map<String,Integer> counts = computeCountsForResources(ring, 100);
        for (String server : counts.keySet()) {
            System.out.println(server + ": " + counts.get(server));
        }
        Map<String,Double> coverage = ring.computeCoverage();
        for (String server : coverage.keySet()) {
            System.out.println(server + ": " + coverage.get(server));
        }
        return counts.get("Manager-" + ring.getAllServerNames().first());
    }

    public static void main (String[] args)
    {
        System.out.println("h: " + hash("foo"));
        System.out.println("h: " + hash("foo1"));
        final int mgrCount = 4;
        final int replicaCount = 5;
        final int resourceCount = 200;
        Percentiles.Stats[] stats = new Percentiles.Stats[mgrCount];
        for (int i = 0; i < stats.length; i++) {
            stats[i] = new Percentiles.Stats();
        }
        for (int i=0; i<1000; ++i) {
            ConsistentHashRing ring = makeHashRing(mgrCount, replicaCount);
            Map<String,Integer> counts = computeCountsForResources(ring, resourceCount);
            int total = 0;
            for (String srvName : counts.keySet()) {
                total += counts.get(srvName);
            }
            if (total != resourceCount) {
                System.out.println("total: " + total);
                assert false;
            }
            String output = "";
            SortedSet<String> names = ring.getAllServerNames();
            int j = 0;
            for (String name : names) {
                Integer val = counts.get(name);
                val = val != null ? val : 0;
                if (stats[j] == null) {
                    assert false;
                }
                output += val + ",";
                stats[j].add(val);
                ++j;
            }
            System.out.println(output);
        }
        for (int i = 0; i < stats.length; i++) {
            System.out.println("stats: " + stats[i].toString());
        }
    }
}
