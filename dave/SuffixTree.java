package dave;

/**
 * User: dfinlay
 * Date: 8/29/13
 */
public class SuffixTree
{
    Node root;

    public static class Node
    {
        char[] chars;
        Node firstChild;

        public Node (char[] string)
        {
            this.chars = string;
        }

        public void add (char[] string)
        {
            if (chars == null || string.length == 0) {
                chars = string;
                return;
            }
            int i=0;
            int len = Math.min(string.length, chars.length);
            for (; i<len && chars[i] == string[i]; ++i) {}
            if (i == chars.length) {
                if (i < string.length) {
                    // new string is super string of old
                    if (firstChild != null) {
                        firstChild = new Node(null);
                    }
                    firstChild.add(trailing(string, i));
                }
            }
            else if (i == string.length) {
                // p
                // c1 -> c2 -> c3
                Node node = new Node(trailing(chars, i));
                node.firstChild = firstChild;
                chars = leading(chars, i);
                firstChild = node;
            }
            else if (i == 0) {
                if (chars[0] < string[0]) {

                }


            }
            //char[] leading = char[]

        }
    }

    public static char[] leading (char[] string, int index)
    {
        char[] extra = new char[index];
        System.arraycopy(string, 0, extra, 0, index);
        return extra;
    }

    public static char[] trailing (char[] string, int index)
    {
        char[] result = new char[string.length - index];
        System.arraycopy(string, index, result, 0, string.length - index);
        return result;
    }

    public void add (char[] string)
    {
        if (root == null) {
            root = new Node(null);
        }
        root.add(string);
    }



}
