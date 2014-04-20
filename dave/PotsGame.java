package dave;

import java.util.ArrayList;
import java.util.List;

/**
    User: dfinlay
    Date: 9/27/12
*/
public class PotsGame
{
    public enum Move {Left, Right}

    public static class MoveResult {
        public List<Move> moves;
        public int value;

        public MoveResult (Move move, int value) {
            this.moves = new ArrayList<Move>();
            if (move != null) {
                this.moves.add(move);
            }
            this.value = value;
        }

        public MoveResult (List<Move> moves, int value) {
            this.moves = moves;
            this.value = value;
        }
    }

    public static MoveResult computeBestMove (int[] pots, int lidx, int ridx)
    {
        if (lidx > ridx) {
            return new MoveResult((Move)null, 0);
        }
        else if (lidx == ridx) {
            return new MoveResult(Move.Left, pots[lidx]);
        }
        MoveResult l = computeBestMove(pots, lidx+1, ridx);
        l.moves.add(0, Move.Left);
        l.value = pots[lidx] - l.value;
        MoveResult r = computeBestMove(pots, lidx, ridx-1);
        r.moves.add(0, Move.Right);
        r.value = pots[ridx] - r.value;
        MoveResult best = l;
        if (r.value > l.value) {
            best = r;
        }
        return best;
    }

    public static void main (String[] args)
    {
        int[] pots = { 1, 3, 1, 2, 3, 2, 5, 3, 2, 1, 5, 3, 2, 1, 3, 4, 5, };
        MoveResult r = computeBestMove(pots, 0, pots.length - 1);
        System.out.println("value: " + r.value);
        System.out.println("moves: " + r.moves);
    }

}
