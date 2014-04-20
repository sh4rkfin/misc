package dave;

import java.util.Arrays;
import java.util.Collection;
import java.util.Comparator;
import java.util.Iterator;

/**
 */
public abstract class SortUtil
{
    public enum Sense
    {
        asc {
            @Override
            public Sense opposite() {
                return desc;
            }
        },
        desc {
            @Override
            public Sense opposite() {
                return asc;
            }
        };
        public abstract Sense opposite ();
    }

    enum Algorithm {
        merge {
            @Override
            public <V> void sort(V[] values, Comparator<V> comparator, Sense sense) {
                mergeSort(values, 0, values.length, comparator, sense);
            }
        },
        qsort {
            @Override
            public <V> void sort(V[] values, Comparator<V> comparator, Sense sense) {
                qsort(values, 0, values.length - 1, comparator, sense);
            }
        },
        heap {
            @Override
            public <V> void sort(V[] values, Comparator<V> comparator, Sense sense) {
                Heap.heapSort(values, comparator);
            }
        };
        public abstract <V> void sort (V[] values, Comparator<V> comparator, Sense sense);
    }

    public static class Stats implements Cloneable
    {
        long comparisons;

        @Override
        protected Object clone () {
            try {
                return super.clone();
            }
            catch (CloneNotSupportedException ex)
            {
                return null;
            }
        }

        public Stats copy () {
            return (Stats)clone();
        }
    }

    private static ThreadLocal<Stats> Stats = new ThreadLocal<Stats>() {
        @Override
        protected Stats initialValue() {
            return new Stats();
        }
    };

    public static Stats currentStats ()
    {
        return Stats.get().copy();
    }

    public static <V> int compare (V first, V second, Comparator<V> comparator, Sense sense)
    {
        Stats.get().comparisons++;
        int result;
        if (comparator == null) {
            result = ((Comparable)first).compareTo(second);
        }
        else {
            result = comparator.compare(first, second);
        }
        return sense == Sense.desc ? -result : result;
    }

    public static <V> int compare (V first, V second)
    {
        return SortUtil.compare(first, second, null, null);
    }

    public static <V> void assertSorted (Collection<V> elements, Comparator<V> comparator, Sense sense)
    {
        V last = null;
        for (V element : elements) {
            Util.assertTrue(last == null || compare(last, element, comparator, sense) <= 0,
                            "last: " + last + ", element: " + element + ": not in " + sense + " order");
            last = element;
        }

    }

    public static <V> void assertSorted (V[] elements, Comparator<V> comparator, Sense sense)
    {
        assertSorted(Arrays.asList(elements), comparator, sense);
    }

    public static <V> void merge (
            Iterator<V>[] sources,
            Sink<V> sink,
            Comparator<V> comparator,
            Sense sense
    )
    throws Exception
    {
        Comparator<ItemSource<V>> comp = ItemSource.makeComparator(comparator);
        Heap<ItemSource<V>> aggregator = new Heap<ItemSource<V>>(null, comp, sense);
        for (Iterator<V> source : sources) {
            if (source.hasNext()) {
                V item = source.next();
                ItemSource<V> si = new ItemSource<V>(source, item);
                aggregator.push(si);
            }
        }
        ItemSource<V> val;
        while ((val = aggregator.peek()) != null) {
            sink.write(val.item);
            if (val.source.hasNext()) {
                val.item = val.source.next();
                aggregator.refreshTop();
            }
            else {
                aggregator.pop();
            }
        }
    }

    public static <V> void swap (V[] values, int i, int j)
    {
        V tmp = values[i];
        values[i] = values[j];
        values[j] = tmp;
    }

    /**
     * Partitions the supplied <code>values</code> array and returns the "pivot" index as follows:
     * <ul>
     * <li> values[i] <= values[pivot] for all i in [lidx, pivot - 1]
     * <li> values[j] >= values[pivot] for all j in [pivot + 1, ridx]
     * </ul>
     * @param values
     * @param lidx
     * @param ridx
     * @param comparator
     * @param sense
     * @param <V>
     * @return
     */
    public static <V> int partition (V[] values, int lidx, int ridx, Comparator<V> comparator, Sense sense)
    {
        if (lidx >= ridx) {
            // done
            return lidx;
        }
        V pivot = values[ridx];
        int i=lidx - 1, j=ridx;
        while (true) {
            while (SortUtil.compare(values[++i], pivot, comparator, sense) < 0) {}
            // all values [lidx, i - 1] < pivot; values[i] >= pivot

            while (SortUtil.compare(values[--j], pivot, comparator, sense) > 0) {
                // values[j] > pivot
                if (j == lidx) {
                    // j has run into lower bound, i.e j == lidx
                    break;
                }
            }
            if (i >= j) break;
            swap(values, i, j);
        }
        swap(values, i, ridx);
        return i;
    }


    /**
     * Quick sorts [lidx, ridx] in place according to the supplied <code>Comparator</code>
     * and <code>sense</code>.
     * @param values
     * @param lidx
     * @param ridx
     * @param comparator
     * @param sense
     * @param <V>
     */
    public static <V> void qsort (V[] values, int lidx, int ridx, Comparator<V> comparator, Sense sense)
    {
        // how does it work?
        // find partition value [3, 2, 5, 2, 3, 1, 4, 1]
        if (lidx >= ridx) {
            // done
            return;
        }
        int pivotIdx = partition(values, lidx, ridx, comparator, sense);
        qsort(values, lidx, pivotIdx - 1, comparator, sense);
        qsort(values, pivotIdx + 1, ridx, comparator, sense);
    }

    public static void testQSort ()
    {
        Integer[] v = {3, 2, 5, 2, 3, 1, 4, 1};
        qsort(v, 0, v.length - 1, null, null);
        assertSorted(v, null, null);
        System.out.println(Arrays.asList(v));
        v = new Integer[] {3, 3, 3, 3, 3, 3, 3, 3};
        qsort(v, 0, v.length - 1, null, null);
    }


    /**
     * Sorts values in alternating fashion. E.g. if sense is asc then the following will be true after
     * the sort:
     *  v[0] <= v[1] >= v2 ...
     * Or equivalently:
     *  v[i] <= v[i+1], i even
     *  v[j] >= v[j+1], j odd
     *
     * @param values
     * @param ridx
     * @param comparator
     * @param sense
     * @param <V>
     */
    public static <V> void sortAlternating (V[] values, int ridx, Comparator<V> comparator, Sense sense)
    {
        if (ridx == 0) {
            return;
        }
        sortAlternating(values, ridx - 1, comparator, sense);
        V r = values[ridx];
        int comp = compare(values[ridx - 1], r, comparator, sense);
        boolean evenr = ridx % 2 == 0;
        if (evenr && comp > 0 || !evenr && comp < 0)  {
            values[ridx] = values[ridx - 1];
            values[ridx - 1] = r;
        }
    }

    public static <V> void assertSortedAlternating (V[] values, Comparator<V> comparator, Sense sense)
    {
        V last = null;
        V current;
        for (int i=0; i<values.length; ++i) {
            current = values[i];
            if (last != null) {
                boolean eveni = i % 2 == 0;
                if (eveni) {
                    Util.assertTrue(compare(last, current, comparator, sense) <= 0,
                            "should be less than last: " + i + ", " + Arrays.asList(values));
                }
                else {
                    Util.assertTrue(compare(last, current, comparator, sense) >= 0,
                            "should be greater than last: " + i + ", " + Arrays.asList(values));
                }
            }
            last = current;
        }

    }

    /**
     * Merges the following 2 ranges of an array:
     * <li> [lidx, midx - 1]
     * <li> [midx, ridx - 1]
     *
     * @param values
     * @param lidx
     * @param midx
     * @param ridx
     * @param comparator
     * @param sense
     * @param <V>
     */
    public static <V> void merge (V[] values, int lidx, int midx, int ridx, Comparator<V> comparator, Sense sense)
    {
        int ti = 0, l0 = lidx, m0 = midx;
        V[] temp = (V[])new Object[ridx - lidx];
        while (lidx < m0 || midx < ridx) {
            if (lidx == m0) {
                temp[ti++] = values[midx++];
            }
            else if (midx == ridx) {
                temp[ti++] = values[lidx++];
            }
            else {
                int cmp = SortUtil.compare(values[lidx], values[midx], comparator, sense);
                temp[ti++] = cmp <= 0 ? values[lidx++] : values[midx++];
            }
        }
        System.arraycopy(temp, 0, values, l0, temp.length);
    }

    /**
     * Sort the sub-array [lidx, ridx - 1] of the <code>values</code> array in place according
     * to the specified comparator and <code>sense</code>.
     * @param values
     * @param lidx
     * @param ridx
     * @param comparator
     * @param sense
     * @param <V>
     */
    public static <V> void mergeSort (V[] values, int lidx, int ridx, Comparator<V> comparator, Sense sense)
    {
        if (ridx - lidx > 1) {
            int half = (ridx - lidx) / 2;
            mergeSort(values, lidx, lidx + half, comparator, sense);
            mergeSort(values, lidx + half, ridx, comparator, sense);
            merge(values, lidx, lidx + half, ridx, comparator, sense);
        }
    }

    public static class ItemSource<W>
    {
        public final Iterator<W> source;
        public W item;

        public ItemSource(Iterator<W> source, W item)
        {
            this.source = source;
            this.item = item;
        }

        public static <W> Comparator<ItemSource<W>> makeComparator (final Comparator<W> comparator)
        {
            return new Comparator<ItemSource<W>> () {
                public int compare (ItemSource<W> first, ItemSource<W> second) {
                    return SortUtil.compare(first.item, second.item, comparator, null);

                }
            };
        }
    }

    public static String[] makeRandomArray (int size)
    {
        final String[] strings = new String[size];
        Sink<char[]> sink = new Sink<char[]>() {
            int idx = 0;
            @Override
            public void write (char[] value)
            {
                strings[idx++] = new String(value);
            }
        };
        try {
            Util.writeRandomCharArrays(strings.length, sink);
        }
        catch (Exception ex) {
            throw new RuntimeException(ex);
        }
        return strings;
    }

    public static void testSort (Algorithm algorithm, String[] strings)
    {
        for (String string : strings) {
            //System.out.println("str: " + string);
        }
        SortUtil.Stats begin = SortUtil.currentStats();
        algorithm.sort(strings, null, null);
        SortUtil.Stats end = SortUtil.currentStats();
        System.out.println("sorted:");
        for (String string : strings) {
            //System.out.println("str: " + string);
        }
        System.out.println("comparisons (" + algorithm + "): " +(end.comparisons - begin.comparisons));
        assertSorted(strings, null, null);
    }

    public static void testSortAlternating()
    {
        for (int i=0; i<50; ++i) {
            String[] strings = makeRandomArray(50);
            sortAlternating(strings, strings.length - 1, null, null);
            assertSortedAlternating(strings, null, null);
        }
    }

    public static void main (String[] args) throws Exception
    {
        int size = 100000;
        String[] strings = makeRandomArray(size);
        testSort(Algorithm.qsort, strings.clone());
        testSort(Algorithm.merge, strings.clone());
        testSort(Algorithm.heap, strings.clone());
        testSortAlternating();
        System.out.println("n * log n: " + (size * Util.log2(size)));

        testQSort();
    }
}
