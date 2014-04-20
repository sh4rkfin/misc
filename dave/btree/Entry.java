package dave.btree;

/**
* Created by IntelliJ IDEA.
* User: dfinlay
* Date: 10/9/12
* Time: 12:42 AM
* To change this template use File | Settings | File Templates.
*/
public class Entry<K,V>
{
    K key;
    V value;
    Node<K,V> child;

    public Entry(K key, V value, Node<K, V> child)
    {
        this.key = key;
        this.value = value;
        this.child = child;
    }

    public Entry(K key, V value)
    {
        this.key = key;
        this.value = value;
    }

    public String toString ()
    {
        return "(" + key + "," + value + ")";
    }
}
