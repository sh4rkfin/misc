package dave;


/**
 * Created by IntelliJ IDEA.
 * User: dfinlay
 * Date: 9/26/12
 * Time: 10:21 PM
 * To change this template use File | Settings | File Templates.
 */
public class HashTab2<K,V>
{
    private K[] keys;
    private V[] values;

    private int _size;
    private float _loadFactor;

    public HashTab2()
    {
        keys = (K[])new Object[17];
        values = (V[])new Object[17];
        _size = 0;
        _loadFactor = 0.75f;
    }

    public V get (K key)
    {
        int pos = key.hashCode() % keys.length;
        while (keys[pos] != null) {
            if (keys[pos].equals(key)) {
                return values[pos];
            }
            pos = (pos + 1) % keys.length;
        }
        return null;
    }

    private void rehashIfNecessary (int newItems)
    {
        int max = (int)(_loadFactor * keys.length);
        if (_size > max) {
            int newSize = (keys.length - 1) * 2 + 1;
            K[] ks = (K[])new Object[newSize];
            V[] vs = (V[])new Object[newSize];
            for (int i=0; i<keys.length; ++i) {
                if (keys[i] != null) {
                    put(ks, vs, keys[i], values[i]);
                }
            }
            keys = ks;
            values = vs;
        }
    }

    public static <K,V> V put (K[] keys, V[] values, K key, V value)
    {
        int pos = key.hashCode() % keys.length;
        while (keys[pos] != null) {
            if (keys[pos].equals(key)) {
                V result = values[pos];
                values[pos] = value;
                keys[pos] = key;
                return result;
            }
            pos = (pos + 1) % keys.length;
        }
        keys[pos] = key;
        values[pos] = value;
        return null;
    }

    public V put (K key, V value)
    {
        rehashIfNecessary(1);
        V result = put(keys, values, key, value);
        ++_size;
        return result;
    }

    public String toString ()
    {
        StringBuffer buf = new StringBuffer("[");
        for (int i=0; i<keys.length; ++i) {
            if (keys[i] != null) {
                buf.append("(" + keys[i] + "," + values[i] + ")");
            }
        }
        buf.append("]");
        return buf.toString();
    }

    public int size ()
    {
        return _size;
    }

    public static void testBasic ()
    {
        HashTab2<String, String> h = new HashTab2<String, String>();
        for (int i=0; i < 100; i++) {
            String s = "a" + i;
            h.put(s, s);
            String v = h.get(s);
            Util.assertTrue(s.equals(v), "put " + s + " in; got " + v + " out");
        }
        System.out.println("h.size: " + h.size());
        System.out.println("h: " + h);
    }

    public static void main (String[] args)
    {
        testBasic();
    }

}
