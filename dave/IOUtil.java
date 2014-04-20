package dave;

import java.io.Writer;

/**
 * Created by IntelliJ IDEA.
 * User: dfinlay
 * Date: 9/23/12
 * Time: 12:52 PM
 * To change this template use File | Settings | File Templates.
 */
public abstract class IOUtil
{


    public static class WriterSink extends Sink<char[]>
    {
        private Writer _writer;

        public WriterSink (Writer writer)
        {
            this._writer = writer;
        }

        @Override
        public void write (char[] value) throws Exception
        {
            _writer.write(value);
            _writer.write('\n');
        }
    }


}
