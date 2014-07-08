package dave;

/**
* Created by IntelliJ IDEA.
* User: dfinlay
* Date: 9/23/12
* Time: 12:49 PM
* To change this template use File | Settings | File Templates.
*/
public abstract class Sink<T>
{
    public abstract void write (T value) throws Exception;
}
