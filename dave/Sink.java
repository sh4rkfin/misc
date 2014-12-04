package dave;

/**
*/
public abstract class Sink<T>
{
    public abstract void write (T value) throws Exception;
}
