package dave.graph;

import dave.Function;

import java.util.*;

/**
* Created by IntelliJ IDEA.
* User: dfinlay
* Date: 10/1/12
* Time: 11:16 AM
* To change this template use File | Settings | File Templates.
*/
public class Node<E>
{
    public enum TraversalState {
        Undiscovered,
        Discovered,
        Processed
    }

    private static Node Sentinel = new Node(null);

    public static Function<Integer> DepthFunction = new Function<Integer>() {
        @Override
        public Integer evaluate(Object... args) {
            return ((Node)args[0]).depth();
        }
    };

    List<Node<E>> neighbors = new ArrayList<Node<E>>();
    E data;

    public Node(E data)
    {
        this.data = data;
    }

    private int depth (Map<Node<E>,Integer> depths)
    {
        if (neighbors == null || neighbors.isEmpty()) {
            return 0;
        }
        Integer depth = depths.get(this);
        if (depth != null) {
            return depth;
        }
        else {
            // -1 will never contribute to a depth. the effect is that
            // the link will contribute to depth but when you find you're in a
            // circle you will stop recursing
            depths.put(this, -1);
        }
        int result = 0;
        if (neighbors != null) {
            result = -1;
            for (Node<E> neighbor : neighbors) {
                int d = neighbor.depth(depths);
                if (d > result) result = d;
            }
        }
        ++result;
        depths.put(this, result);
        return result;
    }

    public int depthNR ()
    {
        LinkedList<Node<E>> nodes = new LinkedList<Node<E>>();
        nodes.add(this);
        HashMap<Node,Integer> depths = new HashMap();
        while (!nodes.isEmpty()) {
            Node<E> current = nodes.getLast();
            Integer d = depths.get(current);
            if (!current.neighbors.isEmpty() && d == null) {
                depths.put(current, -1);
                for (Node<E> n : current.neighbors) {
                    if (depths.get(n) == null) {
                        nodes.add(n);
                    }
                }
            }
            else {
                nodes.removeLast();
                int depth = -1;
                for (Node<E> n : current.neighbors) {
                    d = depths.get(n);
                    if (d > depth) depth = d;
                }
                depths.put(current, ++depth);
            }
        }
        return depths.get(this);
    }

    public int depth ()
    {
        return depth(new HashMap<Node<E>,Integer>());
    }

    public int shortestPath (Map<Node<E>,Integer> pathLengths)
    {
        if (neighbors == null || neighbors.isEmpty()) {
            return 0;
        }
        Integer len = pathLengths.get(this);
        if (len != null) {
            return len;
        }
        else {
            // -1 will never contribute to a depth. the effect is that
            // the link will contribute to depth but when you find you're in a
            // circle you will stop recursing
            pathLengths.put(this, -1);
        }
        int result = -1;
        for (Node<E> neighbor : neighbors) {
            int d = neighbor.shortestPath(pathLengths);
            if (d == -1) {
                // in a loop, count as 0
                d = 0;
            }
            if (d < result || result == -1) {
                result = d;
            }
        }
        ++result;
        pathLengths.put(this, result);
        return result;
    }

    public int shortestPathTo (Map<Node<E>,Integer> pathLengths, E obj)
    {
        if (obj.equals(data)) {
            return 0;
        }
        if (neighbors == null || neighbors.isEmpty()) {
            return Integer.MAX_VALUE;
        }
        Integer len = pathLengths.get(this);
        if (len != null) {
            return len;
        }
        else {
            // -1 will never contribute to a depth. the effect is that
            // the link will contribute to depth but when you find you're in a
            // circle you will stop recursing
            pathLengths.put(this, -1);
        }
        int result = Integer.MAX_VALUE;
        for (Node<E> neighbor : neighbors) {
            int d = neighbor.shortestPathTo(pathLengths, obj);
            if (d == -1) {
                // in a loop, count as 0
                d = Integer.MAX_VALUE;
            }
            if (d < result || result == -1) {
                result = d;
            }
        }
        if (result != Integer.MAX_VALUE) {
            ++result;
        }
        pathLengths.put(this, result);
        return result;
    }

    public int depthLevelOrder ()
    {
        List<Node<E>> list = new java.util.LinkedList<Node<E>>();
        list.add(this);
        list.add(Sentinel);
        int depth = 0;
        while (!list.isEmpty()) {
            Node<E> node = list.remove(0);
            if (node != Sentinel) {
                System.out.println("node: " + node.data + ", depth: " + depth);
                list.addAll(node.neighbors != null ? node.neighbors : Collections.<Node<E>>emptyList());
            }
            else if (!list.isEmpty()) {
                ++depth;
                list.add(Sentinel);
            }
        }
        return depth;
    }

    public void add (Node<E> neighbor)
    {
        neighbors.add(neighbor);
    }

    public String toString ()
    {
        return "[" + data + " (" + neighbors.size() + " neighbors)]";
    }
}
