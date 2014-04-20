package dave.btree;

import dave.SortUtil;

/**
 * User: dfinlay
 * Date: 10/6/12
 * Time: 2:33 PM
 * To change this template use File | Settings | File Templates.
 */
public class BTree<K,V>
{
    public static final int M = 4;

    private Node<K,V> root = new Node<K,V>();
    private int height = 0;

    public V search (K key)
    {
        Entry<K,V> result = root.search(key, height);
        return result != null ? result.value : null;
    }

    private final int compare (K k1, K k2)
    {
        return SortUtil.compare(k1, k2);
    }

    public void insert (K key, V value)
    {
        Node<K,V> newChild = root.insert(key, value, height);
        if (newChild == null) {
            return;
        }
        Node<K,V> newRoot = new Node<K,V>();
        newRoot.count = 2;
        newRoot.entries[0] = new Entry<K,V>(root.firstKey(), null, root);
        newRoot.entries[1] = new Entry<K,V>(newChild.firstKey(), null, newChild);
        root = newRoot;
        height++;
    }

    public static void main (String[] args)
    {
        BTree<String,String> bt = new BTree<String, String>();
        for (int i=0; i<100; ++i) {
            String key = "foo:" + i;
            bt.insert(key, "bar:" + i);
            String found = bt.search(key);
            System.out.println("found: " + found);
        }
    }




}
