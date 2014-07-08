package dave;

import java.util.Comparator;
import java.util.Iterator;
import java.util.Random;

/**
 * User: dfinlay
 * Date: 9/22/12
 * Time: 10:49 AM
 */
public class Heap<T>
{
    // --------------------------------------------------------------------------------------------
    // static methods

    public static int leftChildIdx (int idx)
    {
        return 2 * idx + 1;
    }

    public static int rightChildIdx (int idx)
    {
        return 2 * idx + 2;
    }

    public static int parentIdx (int idx)
    {
        return idx == 0 ? -1 : (idx - 1) / 2;
    }

    public static <V> void siftUp (
            int idx,
            V[] elements,
            Comparator<V> comparator,
            SortUtil.Sense sense
    )
    {
        if (sense == null) {
            sense = SortUtil.Sense.asc;
        }
        int pidx = parentIdx(idx);
        while (pidx >= 0) {
            V obj = elements[idx];
            V pobj = elements[pidx];
            if (SortUtil.compare(obj, pobj, comparator, sense) >= 0) break;
            elements[idx] = pobj;
            elements[pidx] = obj;
            idx = pidx;
            pidx = parentIdx(idx);
        }
    }

    public static <V> void siftDown (
            int idx,
            int size,
            V[] elements,
            Comparator<V> comparator,
            SortUtil.Sense sense
    )
    {
        if (sense == null) sense = SortUtil.Sense.asc;
        int lidx = leftChildIdx(idx);
        while (lidx < size) {
            int ridx = lidx + 1;
            V lobj = elements[lidx];
            int midx = lidx;
            V mobj = lobj;
            if (ridx < size && SortUtil.compare(lobj, elements[ridx], comparator, sense) > 0) {
                midx = ridx;
                mobj = elements[ridx];
            }
            V obj = elements[idx];
            if (SortUtil.compare(obj, mobj, comparator, sense) <= 0) {
                break;
            }
            elements[idx] = mobj;
            elements[midx] = obj;
            idx = midx;
            lidx = leftChildIdx(idx);
        }
    }

    public static <V> void heapSort (V[] elements, Comparator<V> comparator, SortUtil.Sense sense)
    {
        int maxParentIdx = elements.length / 2;
        for (int i=maxParentIdx; i>=0; --i) {
            siftDown(i, elements.length, elements, comparator, sense.opposite());
        }
        int size = elements.length;
        while (size > 0) {
            V val = elements[0];
            elements[0] = elements[size - 1];
            elements[size - 1] = val;
            siftDown(0, --size, elements, comparator, sense.opposite());
        }
    }

    public static <V> void heapSort (V[] elements, Comparator<V> comparator)
    {
        heapSort(elements, comparator, SortUtil.Sense.asc);
    }

    // --------------------------------------------------------------------------------------------
    // nested classes

    public static class Sink<V> extends dave.Sink<V>
    {
        private Heap<V> _heap;

        public Sink (Heap<V> heap)
        {
            this._heap = heap;
        }

        @Override
        public void write (V value)
        {
            _heap.push(value);
        }
    }

    public static class Source<V> implements Iterator<V>
    {
        private Heap<V> _heap;

        public Source (Heap<V> heap) {
            this._heap = heap;
        }

        public V next ()
        {
            return _heap.pop();
        }

        public boolean hasNext()
        {
            return _heap.peek() != null;
        }

        public void remove()
        {
            throw new UnsupportedOperationException("remove not supported");
        }
    }

    // --------------------------------------------------------------------------------------------
    // data members

    private T[] _elements;
    private Comparator<T> _comparator;
    private SortUtil.Sense _sense;
    private int _size;

    // --------------------------------------------------------------------------------------------
    // constructors

    public Heap (T[] array, Comparator<T> comparator, SortUtil.Sense sense)
    {
        _elements = array != null ? array : (T[])new Object[16];
        _size = array != null ? array.length : 0;
        _comparator = comparator;
        _sense = sense;
        for (int i=1; i<_size; ++i) {
            siftUp(i, _elements, _comparator, _sense);
        }
    }

    public Heap (T[] array, Comparator<T> comparator)
    {
        this(array, comparator, null);
    }

    public Heap (Comparator<T> comparator)
    {
        this(null, comparator);
    }

    public Heap()
    {
        this(null);
    }

    // --------------------------------------------------------------------------------------------
    // instance methods

    public String toString ()
    {
        StringBuffer buffer = new StringBuffer();
        buffer.append("[");
        for (int i=0; i<_size; ++i) {
            buffer.append(_elements[i]);
            buffer.append(",");
        }
        buffer.append("]");
        return buffer.toString();
    }

    public void push (T object)
    {
        checkSize(_size + 1);
        _elements[_size] = object;
        siftUp(_size++, _elements, _comparator, _sense);
    }

    public T popAndPush (T toPush)
    {
        T result = _size > 0 ? _elements[0] : null;
        _size = _size == 0 ? 1 : _size;
        _elements[0] = toPush;
        siftDown(0, _size, _elements, _comparator, _sense);
        return result;
    }

    public void refreshTop ()
    {
        siftDown(0, _size, _elements, _comparator, _sense);
    }

    public T peek ()
    {
        return _size > 0 ? _elements[0] : null;
    }

    public T pop ()
    {
        if (_size == 0) {
            return null;
        }
        T result = _elements[0];
        _elements[0] = _elements[_size - 1];
        _elements[_size - 1] = null;
        _size--;
        siftDown(0, _size, _elements, _comparator, _sense);
        return result;
    }

    public int size ()
    {
        return _size;
    }

    public boolean isEmpty ()
    {
        return _size == 0;
    }

    private void checkSize (int newSize)
    {
        if (newSize > _elements.length) {
            T[] elements = (T[])new Object[_elements.length * 2];
            System.arraycopy(_elements, 0, elements, 0, _elements.length);
            _elements = elements;
        }
    }

    public static void testHeapSort () throws Exception
    {
        final String[] strings = new String[100];
        dave.Sink<char[]> sink = new dave.Sink<char[]>() {
            int idx = 0;
            @Override
            public void write (char[] value)
            {
                strings[idx++] = new String(value);
            }
        };
        Util.writeRandomCharArrays(strings.length, sink);
        for (String string : strings) {
            System.out.println("str: " + string);
        }
        heapSort(strings, null);
        System.out.println("sorted:");
        for (String string : strings) {
            System.out.println("str: " + string);
        }

        Integer[] array = new Integer[100];
        Random rnd = new Random();
        for (int i=0; i<array.length; ++i) {
            array[i] = rnd.nextInt(100);
        }
        heapSort(array, null);
        SortUtil.assertSorted(array, null, SortUtil.Sense.asc);
        for (int i = 0; i < array.length; i++) {
            Integer integer = array[i];
            System.out.println("arr: [" + i + "]: " + integer);
        }
    }

    public static void main (String[] args) throws Exception
    {
        testHeapSort();
        Heap<Integer> h = new Heap<Integer>();
        h.push(2);
        h.push(1);
        h.push(4);
        h.push(3);
        System.out.println("heap: " + h);

        Integer val = h.pop();
        while (val != null) {
            System.out.println("h.pop: " + val + ", heap:" + h);
            val = h.pop();
        }

        Random rnd = new Random();
        for (int i=0; i<30; ++i) {
            h.push(rnd.nextInt(100));
        }

        Integer last = null;
        val = h.pop();
        while (val != null) {
            if (last != null && last > val) {
                assert false;
            }
            last = val;
            System.out.println("h.pop: " + val + ", heap:" + h);

            val = h.pop();
        }

        /*
        h = new Heap<Integer>(array, null);
        last = null;
        val = h.pop();
        while (val != null) {
            System.out.println("h.pop: " + val + ", heap:" + h);
            assert last == null || last <= val;
            last = val;
            val = h.pop();
        } */
    }


}
