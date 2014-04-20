package dave.btree;

import dave.SortUtil;

/**
* Created by IntelliJ IDEA.
* User: dfinlay
* Date: 10/9/12
* Time: 12:43 AM
* To change this template use File | Settings | File Templates.
*/
public class Node<K,V>
{
    int count = 0;
    Entry<K,V>[] entries = (Entry<K,V>[])new Entry[BTree.M];

    K firstKey ()
    {
        return entries[0].key;
    }

    Node<K,V> split ()
    {
        Node<K,V> result = new Node<K,V>();
        for (int j=0; j < BTree.M/2; ++j) {
            result.entries[j] = entries[BTree.M/2 + j];
        }
        count = result.count = BTree.M/2;
        return result;
    }

    Entry<K,V> search (K key, int hgt)
    {
        if (hgt == 0) {
            for (Entry<K, V> entry : entries) {
                if (SortUtil.compare(key, entry.key) == 0)
                    return entry;
            }
        }
        else {
            for (int j=0; j < count; ++j) {
                if ((j+1) == count || SortUtil.compare(key, entries[j + 1].key) < 0)
                    return entries[j].child.search(key, hgt - 1);
            }
        }
        return null;
    }

    Node<K,V> insert (K key, V value, int hgt)
    {
        int idx;
        Entry<K,V> newEntry = null;
        if (hgt == 0) {
            for (idx=0; idx < count; ++idx) {
                if (SortUtil.compare(key, entries[idx].key) <  0)
                    break;
            }
            newEntry = new Entry<K,V>(key, value);
        }
        else {
            for (idx=0; idx < count; ++idx) {
                if ((idx+1) == count || SortUtil.compare(key, entries[idx+1].key) < 0) {
                    Node<K,V> newNode = entries[idx].child.insert(key, value, hgt - 1);
                    ++idx;
                    if (newNode == null) {
                        return null;
                    }
                    newEntry = new Entry<K,V>(newNode.firstKey(), null, newNode);
                    break;
                }
            }
        }
        int i;
        for (i=count; i > idx; --i) {
            entries[i] = entries[i-1];
        }
        entries[idx] = newEntry;
        count++;
        if (count < BTree.M) {
            return null;
        }
        return split();
    }

    public String toString ()
    {
        StringBuffer buf = new StringBuffer();
        for (int j = 0; j < count; j++) {
            Entry<K, V> entry = entries[j];
            buf.append(entry + ",");
        }
        return buf.toString();
    }
}
