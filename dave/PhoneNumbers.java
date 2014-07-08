package dave;

/**
* User: dfinlay
* Date: 10/2/12
* Time: 4:56 PM
* To change this template use File | Settings | File Templates.
*/
public class PhoneNumbers
{
    private static char[][] Digit2Letters = new char[10][];
    static
    {
        Digit2Letters[0] = new char[]{'0'};
        Digit2Letters[1] = new char[]{'a','b','c'};
        Digit2Letters[2] = new char[]{'d','e','f'};
        Digit2Letters[3] = new char[]{'g','h','i'};
        Digit2Letters[4] = new char[]{'j','k','l'};
        Digit2Letters[5] = new char[]{'m','n','o'};
        Digit2Letters[6] = new char[]{'p','q','r'};
        Digit2Letters[7] = new char[]{'s','t','u'};
        Digit2Letters[8] = new char[]{'v','w','x'};
        Digit2Letters[9] = new char[]{'y','z'};
    }

    public static void generatePermutations (String phoneNumber, int max)
    {
        int total = 1;
        char[] string = phoneNumber.toCharArray();
        for (int i = 0; i < string.length; i++) {
            int num = Digit2Letters[string[i] - '0'].length;
            total *= num;
        }
        System.out.println("total:" + total);
        for (int i = 0; i < total; i++) {
            char[] generated = new char[string.length];
            int num = i;
            for (int j = 0; j < string.length; j++) {
                char[] alternativeChars = Digit2Letters[string[j] - '0'];
                int idx = num % alternativeChars.length;
                num = num / alternativeChars.length;
                generated[j] = alternativeChars[idx];
            }
            System.out.println(new String(generated));
            if (i > max) break;
        }
    }

    public static int generatePermutationsRecursively (String phoneNumber, char[] array, int lidx, int max)
    {
        if (lidx == array.length || max <= 0) {
            return max;
        }
        char[] alternativeChars = Digit2Letters[phoneNumber.charAt(lidx) - '0'];
        for (int i = 0; i < alternativeChars.length && max >= 0; i++) {
            array[lidx] = alternativeChars[i];
            if (lidx + 1 < array.length) {
                max = generatePermutationsRecursively(phoneNumber, array, lidx + 1, max);
            }
            else {
                System.out.println(new String(array));
                max--;
            }
        }
        return max;
    }

    public static void generatePermutationsRecursively (String phoneNumber)
    {
        generatePermutationsRecursively(phoneNumber, new char[phoneNumber.length()], 0, 100);
    }

    public static void main (String[] args)
    {
        generatePermutationsRecursively("123456789");
    }
}
