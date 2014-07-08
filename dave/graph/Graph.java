package dave.graph;

import java.util.*;

/**
 * Created by IntelliJ IDEA.
 * User: dfinlay
 * Date: 9/26/12
 * Time: 6:37 PM
 * To change this template use File | Settings | File Templates.
 */
public class Graph<T>
{

    public static void testBasic ()
    {
        Node<String> a = new Node<String>("a");
        Node<String> b = new Node<String>("b");
        a.add(b);
        b.add(a);
        System.out.println("a.depth: " + a.depth());
        assert a.depth() == 1;
        System.out.println("a.depthNR: " + a.depthNR());
        assert a.depthNR() == 1;
        a.neighbors.clear();
        b.neighbors.clear();

        Node<String> c = new Node<String>("c");
        Node<String> d = new Node<String>("d");
        Node<String> e = new Node<String>("e");
        a.add(b);
        b.add(c);
        a.add(d);
        d.add(e);
        e.add(b);
        // a -> b -> c
        // |--> d -> e -> b
        System.out.println("a.depth:" + a.depth());
        assert a.depth() == 4;
        System.out.println("a.depthNR: " + a.depthNR());
        e.neighbors.clear();
        e.add(b);
        int depth = a.depthLevelOrder();
        System.out.println("a.depthLO: " + depth);
        e.add(a);
        System.out.println("a.shortestPath: " + a.shortestPath(new HashMap()));

    }


    public static void main (String[] args)
    {
        testBasic();
    }
}
