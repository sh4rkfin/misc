package dave;

import java.io.*;
import java.math.BigInteger;
import java.util.*;

/**
 * Created by IntelliJ IDEA.
 * User: dfinlay
 * Date: 9/22/12
 * Time: 4:45 PM
 * To change this template use File | Settings | File Templates.
 */
public abstract class Util
{
    public static int sgn (int first, int second)
    {
        return first < second ? -1 : (first > second ? 1 : 0);
    }

    public static int sgn (long first, long second)
    {
        return first < second ? -1 : (first > second ? 1 : 0);
    }

    public static int log2 (long number)
    {
        int result = -1;
        while (number > 0) {
            number >>>= 1;
            result++;
        }
        return result;
    }

    public static class AssertionFailure extends RuntimeException
    {
        public AssertionFailure (String s, Throwable throwable)
        {
            super(s, throwable);
        }
    }

    public static <T> T max (Iterator<T> values, Comparator<T> comparator)
    {
        if (!values.hasNext()) {
            return null;
        }
        T result = values.next();
        while (values.hasNext()) {
            T value = values.next();
            if (SortUtil.compare(value, result, comparator, null) > 0) {
                result = value;
            }
        }
        return result;
    }


    public static void assertTrue (boolean value, String message)
    {
        if (!value) throw new AssertionFailure(message, null);
    }

    public static char[] nextCharArray (int charCount, Random rnd)
    {
        if (rnd == null) {
            rnd = new Random();
        }
        char[] result = new char[charCount];
        for (int i=0; i<charCount; ++i) {
            result[i] = (char)('a' + rnd.nextInt(26));
        }
        return result;
    }

    public static String nextString (int charCount, Random rnd)
    {
        return new String(nextCharArray(charCount, rnd));
    }

    public static void writeRandomCharArrays(int n, int charArraySize, Sink<char[]> sink)
    throws Exception
    {
        Random rnd = new Random();
        for (int i=0; i<n; ++i) {
            sink.write(nextCharArray(charArraySize, rnd));
        }
    }

    public static void writeRandomCharArrays(int n, Sink<char[]> sink)
    throws Exception
    {
        writeRandomCharArrays(n, 3, sink);
    }

    public static void writeRandomStringFile (String fileName, int lineCount)
    throws Exception
    {
        Writer w = new BufferedWriter(new OutputStreamWriter(
                new FileOutputStream(fileName)));
        Sink<char[]> sink = new IOUtil.WriterSink(w);
        try {
            writeRandomCharArrays(lineCount, sink);
        }
        finally {
            w.flush();
            w.close();
        }
    }

    public static void testSortOneFile (String[] args)
    throws Exception
    {
        writeRandomStringFile("terms.txt", 1000);
        LineNumberReader reader = new LineNumberReader(new BufferedReader(
                new InputStreamReader(new FileInputStream("terms.txt"))));
        String line = reader.readLine();
        Heap<String> h = new Heap<String>();
        while (line != null) {
            h.push(line);
            line = reader.readLine();
        }
        String val = h.pop();
        while (val != null) {
            System.out.println("val: " + val);
            val = h.pop();
        }
    }

    public static Iterator<String> getSortedStringIterators(int itemCount) throws Exception
    {
        final Heap<String> h = new Heap<String>();
        Sink<char[]> s = new Sink<char[]>() {
            @Override
            public void write(char[] value) {
                h.push(new String(value));
            }
        };
        writeRandomCharArrays(itemCount, s);
        return new Heap.Source<String>(h);
    }

    public static final double SqrtTolerance = 1e-12;

    public static double sqrt (double x)
    {
        double z = 1.0;
        double delta;
        int count = 0;
        while (Math.abs((delta = (z - x / z) / 2)) > SqrtTolerance) {
            z = z - delta;
            if (count > 20) break;
            count++;
        }
        return z;
    }

    private static void testSort () throws Exception
    {
        Iterator<String>[] sources = (Iterator<String>[])new Iterator[10];
        for (int i=0; i<sources.length; ++i) {
            sources[i] = getSortedStringIterators(1000);
        }

        final List<String> top = new ArrayList();
        Sink<String> sink = new Sink<String>() {
            private int count = 0;
            @Override
            public void write (String value) {
                if (count++ < 100) {
                    System.out.println(value);
                }
                top.add(value);
            }
        };

        SortUtil.merge(sources, sink, null, SortUtil.Sense.asc);
        SortUtil.assertSorted(top, null, null);
    }

    public static void testBigIntThing ()
    {
        long l = 1L << 32;
        System.out.println(l);
        BigInteger bi = new BigInteger(Long.toString(l-1));
        BigInteger bi2 = new BigInteger(Long.toString(l));
        System.out.println(bi);
        System.out.println(bi2);
    }

    public static void main (String[] args)
    throws Exception
    {
        double x = 2;
        System.out.println("sqrt1: " + sqrt(x));
        System.out.println("sqrt1: " + sqrt(x) * sqrt(x));
        System.out.println("sqrt2: " + Math.sqrt(x));
        System.out.println("sqrt2: " + Math.sqrt(x) * Math.sqrt(x));
    }
}

