package dave;

import java.util.Comparator;
import java.util.Random;

/**
 * User: dfinlay
 * Date: 9/25/12
 * Time: 8:27 AM
 */
public class LinkedList<T>
{
    public static class Node<V>
    {
        public static <V> Node<V> mergeRec (Node<V> n, Node<V> m, Comparator<V> comparator, SortUtil.Sense sense)
        {
            if (n == null) {
                return m;
            }
            if (m == null) {
                return n;
            }
            if (SortUtil.compare(n.data, m.data, comparator, sense) > 0) {
                // n is less than m
                Node<V> tmp = n;
                n = m;
                m = tmp;
            }
            n.next = mergeRec(n.next, m, comparator, sense);
            return n;
        }

        public static <V> Node merge (Node<V> n, Node<V> m, Comparator<V> comparator, SortUtil.Sense sense)
        {
            if (m == null) {
                return n;
            }
            Node<V> result = new Node<V>(null), nprev, mprev;
            result.next = n;
            n = result;
            while (true) {
                // advance until: n.data > m.data
                do {
                    nprev = n;
                    n = n.next;
                } while (n != null && SortUtil.compare(n.data, m.data, comparator, sense) <= 0);
                if (n == null) {
                    // done
                    break;
                }
                Node<V> mstart = m;
                do {
                    mprev = m;
                    m = m.next;
                }
                while (m != null && SortUtil.compare(n.data, m.data, comparator, sense) > 0);
                nprev.next = mstart;
                mprev.next = n;
                if (m == null) {
                    // done
                    break;
                }
            }
            if (n == null) {
                nprev.next = m;
            }

            return result.next;
        }

        public static <V> Node<V> mergeSort (Node<V> n, Comparator<V> comparator, SortUtil.Sense sense)
        {
            Node<V> m = n.getMiddle();
            if (m == n && m.next == null) {
                return n;
            }
            Node<V> m1 = m;
            m = m.next;
            m1.next = null;
            return merge(mergeSort(n, comparator, sense), mergeSort(m, comparator, sense), comparator, sense);
        }

        public static <V> Node<V> mergeSortBU (Node<V> n, Comparator<V> comparator, SortUtil.Sense sense)
        {
            Node<V> head = n;
            for (int w=1; true; w=w*2) {
                Node<V> current = head;
                boolean firstMerge = true;
                while (current != null) {
                    Node<V> n1 = current;
                    current = current.removeHead(w);
                    Node<V> n2 = current;
                    if (current != null) {
                        current = current.removeHead(w);
                    }
                    Node<V> merged = merge(n1, n2, comparator, sense);
                    if (firstMerge) {
                        head = merged;
                        if (current == null) {
                            return head;
                        }
                    }
                    else {
                        head.append(merged);
                    }
                    firstMerge = false;
                }
            }
        }

        public Node<V> next;

        public V data;

        public Node (V data)
        {
            this.data = data;
        }

        public void append (Node<V> node)
        {
            Node<V> n = this;
            for (; n.next != null; n = n.next) {}
            n.next = node;
        }

        public int size ()
        {
            int result = 0;
            for (Node<V> n=this; n != null; n=n.next) {
                ++result;
            }
            return result;
        }

        public String toString ()
        {
            StringBuffer result = new StringBuffer("[");
            for (Node<V> n=this; n != null; n=n.next) {
                result.append(n.data);
                result.append(',');
            }
            result.append(']');
            return result.toString();
        }

        public Node<V> getMiddle ()
        {
            Node<V> slow = this, fast = this;
            while (true) {
                fast = fast.next != null ? fast.next.next : null;
                if (fast == null) break;
                slow = slow.next;
            }
            return slow;
        }

        public Node<V> get (int idx)
        {
            Node<V> n = this;
            for (int i=0; i < idx && n.next != null; ++i) {
                n = n.next;
            }
            return n;
        }

        public Node<V> removeHead (int count)
        {
            Node<V> n = this;
            for (int i=0; i < count-1 && n.next != null; ++i) {
                n = n.next;
            }
            Node<V> result = null;
            if (n != null) {
                result = n.next;
                n.next = null;
            }
            return result;
        }

    }

    private Node<T> head;

    public LinkedList()
    {
        head = null;
    }

    public void add (T object)
    {
        Node<T> node = new Node<T>(object);
        if (head == null) {
            head = node;
        }
        else {
            head.append(node);
        }
    }

    public int size ()
    {
        return head != null ? head.size() : 0;
    }


    public static void main (String[] args)
    {
        LinkedList<String> list = new LinkedList<String>();
        list.add("aa");
        list.add("cc");
        System.out.println(list.head);
        System.out.println("list.size: " + list.size());
        LinkedList<String> l2 = new LinkedList<String>();
        l2.add("bb");
        l2.add("bb1");
        l2.add("dd");
        Node<String> result = Node.merge(list.head, l2.head, null, SortUtil.Sense.asc);
        System.out.println(result);
        System.out.println(result.getMiddle());

        System.out.println((result = Node.mergeSort(result, null, SortUtil.Sense.desc)));
        System.out.println((result = Node.mergeSort(result, null, SortUtil.Sense.asc)));
        System.out.println((result = Node.mergeSortBU(result, null, SortUtil.Sense.desc)));
        System.out.println((result = Node.mergeSortBU(result, null, SortUtil.Sense.asc)));

        LinkedList<Integer> ints = new LinkedList<Integer>();
        Random rnd = new Random();
        for (int i=0; i<50; ++i) {
            ints.add(rnd.nextInt(100));
        }
        System.out.println((ints.head = Node.mergeSortBU(ints.head, null, SortUtil.Sense.asc)));
        System.out.println((ints.head = Node.mergeSortBU(ints.head, null, SortUtil.Sense.desc)));
    }
}
