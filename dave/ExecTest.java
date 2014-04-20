package dave;

import java.io.BufferedInputStream;
import java.io.File;
import java.io.InputStreamReader;
import java.io.Reader;

/**
    User: dfinlay
    Date: 9/26/12
*/
public abstract class ExecTest
{

    public static void main (String[] args)
    throws Exception
    {
        File dir = new File("./out/production/misc");
        p.Process p = Runtime.getRuntime().exec("java dave.Test", null, dir);
        Reader r = new InputStreamReader(new BufferedInputStream(p.getInputStream()));
        char[] buf = new char[8];
        int read;
        while ((read = r.read(buf)) >= 0) {
            System.out.print("input: " + new String(buf, 0, read));
        }
        Reader e = new InputStreamReader(new BufferedInputStream(p.getErrorStream()));
        while ((read = e.read(buf)) >= 0) {
            System.out.print("err: " + new String(buf, 0, read));
        }
    }

}
