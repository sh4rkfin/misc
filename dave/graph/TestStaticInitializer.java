package dave.graph;

/**
 * User: dfinlay
 * Date: 10/2/13
 */
public class TestStaticInitializer {

    public TestStaticInitializer ()
    {}

    static {
        TestStaticInitializer foo = new TestStaticInitializer();
    }
}
