package dave;

import org.w3c.dom.css.Counter;

import java.util.*;

/**
 * User: dfinlay
 * Date: 8/23/13
 */
public abstract class TopN
{
    private static ThreadLocal<SortUtil.Stats> Stats = new ThreadLocal<SortUtil.Stats>() {
        @Override
        protected SortUtil.Stats initialValue() {
            return new SortUtil.Stats();
        }
    };

    public static SortUtil.Stats currentStats ()
    {
        return Stats.get().copy();
    }

    public static class Count<T>
    {
        T instance;
        int count;

        public Count(T instance, int count) {
            this.instance = instance;
            this.count = count;
        }

        public Count(T instance) {
            this(instance, 0);
        }

        public static final Comparator<Count> CountComp = new Comparator<Count>() {
            public int compare(Count first, Count second) {
                Stats.get().comparisons++;
                return Util.sgn(first.count, second.count);
            }
        };

        public static <T> Comparator<Count<T>> countComparator () {
            return (Comparator<Count<T>>)(Object)CountComp;
        }

        @Override
        public String toString() {
            return instance + ": " + count;
        }

        @Override
        public int hashCode () {
            return instance.hashCode() + (count * 31);
        }

        @Override
        public boolean equals (Object that) {
            return that != null && equals((Count<String>)that);
        }

        public boolean equals (Count<String> that) {
            return that != null && this.instance.equals(that.instance) && this.count == that.count;
        }
    }

    public static List<Count<String>> getTopNWithHeap (int n, Collection<Count<String>> values)
    {
        SortUtil.Stats start = currentStats();
        Heap<Count<String>> h = new Heap<Count<String>>(null,
                                                        Count.<String>countComparator(),
                                                        SortUtil.Sense.asc);
        for (Count<String> current : values) {
            //System.out.println(current);
            if (h.size() < n) {
                h.push(current);
            }
            else {
                Count<String> top = h.peek();
                if (Count.CountComp.compare(top, current) < 0) {
                    h.popAndPush(current);
                }
            }
        }
        SortUtil.Stats end = currentStats();
        System.out.println("comps:" + (end.comparisons - start.comparisons));
        List<Count<String>> result = new ArrayList<Count<String>>();
        while (!h.isEmpty()) {
            result.add(h.pop());
        }
        return result;
    }

    public static List<Count<String>> getTopNWithTree (int n, Collection<Count<String>> values)
    {
        final Comparator<Count<String>> comp = new Comparator<Count<String>>() {
            public int compare(Count<String> first, Count<String> second) {
                int result = Count.CountComp.compare(first, second);
                return result != 0 ? result : first.instance.compareTo(second.instance);
            }
        };

        SortUtil.Stats start = currentStats();
        TreeSet<Count<String>> tree = new TreeSet<Count<String>>(comp);
        for (Count<String> current : values) {
            if (tree.size() < n) {
                tree.add(current);
            }
            else {
                Count<String> top = tree.first();
                if (Count.CountComp.compare(top, current) < 0) {
                    tree.pollFirst();
                    tree.add(current);
                }
            }
        }
        SortUtil.Stats end = currentStats();
        long diff = end.comparisons - start.comparisons;
        System.out.println("comps:" + diff + ", " + (diff * 1.0/values.size()));
        System.out.println("n*logn: " + Util.log2(values.size()) * values.size());
        List<Count<String>> result = new ArrayList<Count<String>>();
        result.addAll(tree);
        for (Count<String> stringCount : result) {
            //System.out.println(stringCount);
        }
        return result;
    }

    public static void testTopN (int n, int numberOfSamples) throws Exception
    {
        final HashMap<String,Count<String>> counts = new HashMap();

        Sink<char[]> sink = new Sink<char[]>() {
            @Override
            public void write(char[] value) {
                String s = new String(value);
                Count count = counts.get(s);
                if (count == null) {
                    count = new Count<String>(s);
                    counts.put(s, count);
                }
                count.count++;
            }
        };

        Util.writeRandomCharArrays(numberOfSamples, 4, sink);

        List<Count<String>> topN = getTopNWithHeap(n, counts.values());
        List<Count<String>> topN2 = getTopNWithTree(n, counts.values());
        System.out.println("topN: " + topN.get(0).count);
        System.out.println("topN2: " + topN2.get(0).count);

        System.out.println("n*logn: " + Util.log2(numberOfSamples) * numberOfSamples);

    }

    public static void main (String[] args) throws Exception
    {
        testTopN(100, 10000000);
    }
}
