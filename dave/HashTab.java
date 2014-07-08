package dave;

import java.util.Iterator;
import java.util.Map;

/**
 * User: dfinlay
 * Date: 9/26/12
 * Time: 9:36 PM
 */
public class HashTab<K,V>
{
    public static class Element<K,V> implements Map.Entry<K,V>
    {
        K key;
        V value;
        Element<K,V> successor;
        Element<K,V> prev;
        Element<K,V> next;

        public Element (K key, V value)
        {
            this.key = key;
            this.value = value;
        }

        public K getKey ()
        {
            return key;
        }

        public V getValue ()
        {
            return value;
        }

        public V setValue (V v)
        {
            V result = value;
            value = v;
            return result;
        }

        public String toString ()
        {
            return "(" + key + "," + value + ")," + (successor != null ? successor.toString() : "");
        }
    }

    public static int hash (int h)
    {
        // This function ensures that hashCodes that differ only by
        // constant multiples at each bit position have a bounded
        // number of collisions (approximately 8 at default load factor).
        h ^= (h >>> 20) ^ (h >>> 12);
        return h ^ (h >>> 7) ^ (h >>> 4);
    }

    private Element<K,V>[] _elements;
    private Element<K,V> _first;
    private Element<K,V> _last;
    private float _loadFactor;
    private int _max;
    private int _size;

    public HashTab ()
    {
        _elements = (Element<K,V>[])new Element[17];
        _loadFactor = 0.75f;
        _max = (int) (_loadFactor * _elements.length);
        _size = 0;
    }

    private HashTab (Element<K,V>[] elements, float loadFactor)
    {
        _elements = elements;
        _loadFactor = loadFactor;
        _max = (int) (_loadFactor * _elements.length);
        _size = 0;
    }

    public V get (K key)
    {
        int pos = hash(key.hashCode()) % _elements.length;
        Element<K,V> e = _elements[pos];
        while (e != null) {
            if (key == e.key || (key != null && key.equals(e.key))) {
                return e.value;
            }
            e = e.successor;
        }
        return null;
    }

    private V _put (K key,V value)
    {
        int pos = hash(key.hashCode()) % _elements.length;
        Element<K,V> elem = _elements[pos];
        Element<K,V> prev = null;
        while (elem != null) {
            if (key == elem.key || (key != null && key.equals(elem.key))) {
                V result = elem.value;
                elem.value = value;
                // just to be sure
                elem.key = key;
                return result;
            }
            prev = elem;
            elem = elem.successor;
        }
        _size++;
        Element<K,V> assoc = new Element<K,V>(key, value);
        if (prev == null) {
            _elements[pos] = assoc;
        }
        else {
            prev.successor = assoc;
        }
        if (_last == null) {
            _first = _last = assoc;
        }
        else {
            _last.next = assoc;
            assoc.prev = _last;
            _last = assoc;
        }
        return null;
    }

    public Iterator<Map.Entry<K,V>> entrySetIterator ()
    {
        final Element<K,V> first = _first;
        return new Iterator<Map.Entry<K, V>>() {
            Element<K,V> _current = first;
            public boolean hasNext() {
                return _current != null;
            }

            public Map.Entry<K, V> next()
            {
                Element<K,V> result = _current;
                if (_current != null) {
                    _current = _current.next;
                }
                return result;
            }

            public void remove() {
                throw new UnsupportedOperationException("remove not supported");
            }
        };
    }

    public V put (K key,V value)
    {
        rehashIfNecessary(1);
        return _put(key, value);
    }

    public V remove (K key)
    {
        int pos = hash(key.hashCode()) % _elements.length;
        Element<K,V> elem = _elements[pos];
        Element<K,V> prev = null;
        while (elem != null) {
            if (key == elem.key || (key != null && key.equals(elem.key))) {
                V result = elem.value;
                if (prev == null) {
                    _elements[pos] = elem.successor;
                }
                else {
                    prev.successor = elem.successor;
                }
                if (elem.prev != null) {
                    elem.prev.next = elem.next;
                }
                else {
                    _first = elem.next;
                }
                if (elem.next != null) {
                    elem.next.prev = elem.prev;
                }
                else {
                    _last = elem.prev;
                }
                --_size;
                return result;
            }
            prev = elem;
            elem = elem.successor;
        }
        return null;

    }

    public int size ()
    {
        return _size;
    }

    private void rehashIfNecessary (int newItems)
    {
        if (_size + newItems < _max) {
            return;
        }
        int newSize = (_elements.length - 1) * 2 + 1;
        Element<K,V>[] elements = (Element<K,V>[])new Element[newSize];
        HashTab tmp = new HashTab(elements, _loadFactor);
        for (Element<K, V> element : _elements) {
            while (element != null) {
                tmp._put(element.key, element.value);
                element = element.successor;
            }
        }
        _elements = tmp._elements;
        _max = tmp._max;
    }

    public String toString ()
    {
        StringBuffer buf = new StringBuffer("[");
        for (Element<K, V> element : _elements) {
            if (element != null) {
                buf.append(element);
            }
        }
        buf.append("]");
        return buf.toString();
    }

    public static void testBasic ()
    {
        HashTab<String, String> h = new HashTab<String, String>();
        for (int i=0; i < 100; i++) {
            String s = "a" + i;
            Util.assertTrue(h.get(s) == null, "h.get() should be null before put");
            int size = h.size();
            h.put(s, s);
            Util.assertTrue((size + 1) == h.size(), "size should have increased by 1");
            String v = h.get(s);
            Util.assertTrue(s.equals(v), "put " + s + " in; got " + v + " out");
            String s2 = h.get(s);
            Util.assertTrue(s.equals(s2), "h.put() not equal to h.get() for key " + s);
            if (i % 2 == 0) {
                String s3 = h.remove(s);
                Util.assertTrue(s.equals(s3), "h.put() not equal to h.remove() for key " + s);
            }
        }
        System.out.println("h.size: " + h.size());
        System.out.println("h: " + h);
        h = new HashTab<String,String>();
        for (int i=0; i < 100; i++) {
            String s = "a" + i;
            h.put(s, s);
            String v = h.get(s);
            Util.assertTrue(s.equals(v), "put " + s + " in; got " + v + " out");
            String s2 = h.get(s);
            Util.assertTrue(s.equals(s2), "h.put() not equal to h.get() for key " + s);
            String s3 = h.remove(s);
            Util.assertTrue(s.equals(s3), "h.put() not equal to h.remove() for key " + s);
        }
    }

    public static void testOrder ()
    {
        HashTab<String, String> h = new HashTab<String, String>();
        for (int i=0; i < 10; i++) {
            String s = "a" + i;
            Util.assertTrue(h.get(s) == null, "h.get() should be null before put");
            int size = h.size();
            h.put(s, s);
            Util.assertTrue((size + 1) == h.size(), "size should have increased by 1");
            String v = h.get(s);
            Util.assertTrue(s.equals(v), "put " + s + " in; got " + v + " out");
            String s2 = h.get(s);
            Util.assertTrue(s.equals(s2), "h.put() not equal to h.get() for key " + s);
            if (i % 2 == 1) {
                String s3 = h.remove(s);
                Util.assertTrue(s.equals(s3), "h.put() not equal to h.remove() for key " + s);
            }
        }
        Iterator<Map.Entry<String,String>> iter = h.entrySetIterator();
        while (iter.hasNext()) {
            System.out.println("entry:" + iter.next());
        }
    }

    public static void main (String[] args)
    {
        testBasic();
        testOrder();
    }

}
