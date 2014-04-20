package dave;

import java.util.Comparator;
import java.util.Iterator;

/**
 * Created by IntelliJ IDEA.
 * User: dfinlay
 * Date: 9/26/12
 * Time: 7:17 PM
 * To change this template use File | Settings | File Templates.
 */
public abstract class Function<T>
{
    public abstract T evaluate (Object... args);

    public static <T,V extends Comparable<? super V>> V max (Iterator<T> values, Function<V> function)
    {
        if (!values.hasNext()) {
            return null;
        }
        V result = function.evaluate(values.next());
        while (values.hasNext()) {
            V value = function.evaluate(values.next());
            if (value.compareTo(result) > 0) {
                result = value;
            }
        }
        return result;
    }
}
