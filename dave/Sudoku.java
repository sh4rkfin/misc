package dave;

/**
    A Sudoku solver.
*/
public class Sudoku
{
    //-----------------------------------------------------------------------------------
    // nested class

    public static class Stats
    {
        public int multiChoiceCount = 0;
        public int deadEnds = 0;

        public String toString ()
        {
            return "multi-choice count: " + multiChoiceCount + ", " +
                    "deadends: " + deadEnds;
        }
    }


    //-----------------------------------------------------------------------------------
    // static methods

    public static int bitCount (int value)
    {
        return BitCount[(value & First3)] +
               BitCount[(value & Mid3) >> 3] +
               BitCount[(value & Last3) >> 6];
    }

    public static int computeValues (int[] array)
    {
        int values = 0;
        for (int val : array) {
            if (val != 0) {
                values |= (1 << (val - 1));
            }
        }
        return values;
    }


    //-----------------------------------------------------------------------------------
    // constants

    private static final byte[] BitCount;
    public static final int All = (1 << 9) - 1;
    public static final int First3 = 7;
    public static final int Mid3   = (First3 << 3);
    public static final int Last3  = (First3 << 6);

    static {
                           //  0, 1, 10, 11, 100, 101, 110, 111
        BitCount = new byte[] {0, 1,  1,  2,   1,   2,   2,   3};
    }

    //-----------------------------------------------------------------------------------
    // data members

    private int[][] grid;
    private int[][] candidates;
    private int[] rows;
    private int[] cols;
    private int[][] boxes;
    private Stats stats = new Stats();

    //-----------------------------------------------------------------------------------
    // constructors

    public Sudoku (int[][] grid)
    {
        this.grid = grid.clone();
        candidates = new int[9][9];
        rows = new int[9];
        cols = new int[9];
        boxes = new int[3][3];
        for (int i=0; i<9; ++i) {
            rows[i] = cols[i] = boxes[i%3][i/3] = -1;
        }
    }

    public Sudoku ()
    {
        this(new int[9][9]);
    }

    //-----------------------------------------------------------------------------------
    // public methods

    public void setValue (int row, int col, int value)
    {
        Util.assertTrue(value >= 0 && value < 10, "invalid value: " + value);
        grid[row][col] = value;
        rows[row] = -1;
        cols[col] = -1;
        boxes[row/3][col/3] = -1;
    }

    public int rowValues (int i)
    {
        if (rows[i] == -1) {
            rows[i] = computeValues(grid[i]);
        }
        return rows[i];
    }

    public int colValues (int j)
    {
        if (cols[j] != -1) {
            return cols[j];
        }
        int values = 0;
        for (int i = 0; i < grid.length; i++) {
            int vij = grid[i][j];
            if (vij != 0) {
                values |= (1 << (vij - 1));
            }
        }
        cols[j] = values;
        return values;
    }

    public int boxValues(int i, int j)
    {
        if (boxes[i][j] != -1) {
            return boxes[i][j];
        }
        int values = 0;
        for (int k = 0; k < grid.length; k++) {
            int vijk = grid[i*3 + k/3][j*3 + k%3];
            if (vijk != 0) {
                values |= (1 << (vijk - 1));
            }
        }
        boxes[i][j] = values;
        return values;
    }

    public boolean solve ()
    {
        // find cell with min number of candidate options
        int mini=-1, minj=-1, bitcount=10, cands=All;
        for (int i = 0; bitcount > 1 && i < grid.length; ++i) {
            for (int j = 0; bitcount > 1 && j < grid.length; j++) {
                if (grid[i][j] == 0) {
                    int taken = rowValues(i) | colValues(j) | boxValues(i/3,j/3);
                    candidates[i][j] = taken ^ All;
                    int bits = bitCount(candidates[i][j]);
                    if (bits < bitcount) {
                        mini = i;
                        minj = j;
                        cands = candidates[i][j];
                        bitcount = bits;
                    }
                }
            }
        }
        if (mini == -1) {
            // check solution and return true if good and false otherwise
            for (int i = 0; i < grid.length; ++i) {
                if (rowValues(i) != All | colValues(i) != All | boxValues(i%3,i/3) != All) {
                    return false;
                }
            }
            return true;
        }
        else if (bitcount == 0) {
            ++stats.deadEnds;
            return false;
        }
        else if (bitcount > 1) {
            ++stats.multiChoiceCount;
        }
        for (int i=0; i<grid.length; ++i) {
            if ((cands & (1 << i)) > 0) {
                setValue(mini, minj, i + 1);
                if (solve()) {
                    return true; // done
                }
                setValue(mini, minj, 0);
            }
        }
        // had candidates but none worked out, backtrack
        return false;
    }

    public String toString ()
    {
        StringBuffer result = new StringBuffer();
        for (int i = 0; i < grid.length; i++) {
            //result.append("row[" + i + "]: ");
            for (int j = 0; j < grid.length; ++j) {
                result.append(grid[i][j]);
                result.append(" ");
                if ((j % 3) == 2 && j != 8) {
                    result.append("|");
                }
            }
            result.append('\n');
            if ((i % 3) == 2 && i != 8) {
                result.append("-------------------\n");
            }
        }
        return result.toString();
    }

    //-----------------------------------------------------------------------------------
    // test stuff

    public static void testBasic ()
    {
        Sudoku s = new Sudoku();
        s.setValue(0,0,4);
        s.setValue(3,5,1);
        System.out.println("s\n" + s);
        s.solve();
        System.out.println("s\n" + s);
    }

    public static void testHard ()
    {
        int[][] puzzle = {
                { 9, 0, 1, 0, 7, 6, 0, 0, 0},
                { 3, 0, 0, 0, 0, 0, 0, 0, 0},
                { 5, 0, 4, 0, 0, 0, 7, 0, 0},
                { 0, 6, 0, 2, 0, 0, 1, 0, 3},
                { 0, 0, 0, 4, 0, 7, 0, 0, 0},
                { 4, 0, 3, 0, 0, 1, 0, 7, 0},
                { 0, 0, 2, 0, 0, 0, 4, 0, 8},
                { 0, 0, 0, 0, 0, 0, 0, 0, 1},
                { 0, 0, 0, 5, 1, 0, 9, 0, 7},
        };
        Sudoku s = new Sudoku(puzzle);
        System.out.println("s:\n" + s);
        boolean solved = s.solve();
        System.out.println("solved: " + solved);
        System.out.println("s:\n" + s);
        System.out.println("stats: " + s.stats);
        Util.assertTrue(solved, "testHard puzzle not solved!");
    }

    public static void testHard2 ()
    {
        int[][] puzzle = {
                { 0, 0, 0, 0, 2, 7, 0, 0, 0},
                { 1, 0, 0, 3, 6, 0, 0, 0, 0},
                { 4, 7, 0, 1, 0, 0, 0, 0, 9},
                { 7, 9, 0, 0, 0, 0, 8, 0, 0},
                { 0, 3, 0, 0, 0, 0, 0, 9, 0},
                { 0, 0, 6, 0, 0, 0, 0, 7, 5},
                { 9, 0, 0, 0, 0, 3, 0, 4, 6},
                { 0, 0, 0, 0, 9, 4, 0, 0, 1},
                { 0, 0, 0, 5, 8, 0, 0, 0, 0},
        };
        Sudoku s = new Sudoku(puzzle);
        System.out.println("s:\n" + s);
        boolean solved = s.solve();
        System.out.println("solved:" + solved);
        System.out.println("s:\n" + s);
        System.out.println("stats: " + s.stats);
        Util.assertTrue(solved, "testHard2 puzzle not solved!");
    }

    public static void main (String[] args)
    {
        int[] row = {4, 5, 1, 2, 3, 7, 9, 8, 6};
        int values = computeValues(row);
        System.out.println("bits: " + Integer.toString(values, 2));

        testBasic();
        testHard();
        testHard2();
    }
}
