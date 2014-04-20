package dave;

import java.util.HashSet;

/**
 * User: dfinlay
 * Date: 5/30/13
 */
public class Network {

    public static class Company
    {
        int id;
        java.util.List<Company> payees = new java.util.ArrayList();
        Company (int id) {
            this.id = id;
        }
        void addPayee (Company c)
        {
            payees.add(c);
        }
        public String toString ()
        {
            String result = "Company: " + id + "(";
            for (Company payee : payees){
                result += payee.id + ",";
            }
            return result + ")";
        }
    }

    java.util.HashMap<Integer,Company> companies = new java.util.HashMap();

    Company find (int id)
    {
        return companies.get(id);
    }

    Company findOrCreate (int id)
    {
        Company result = find(id);
        if (result == null) {
            result = new Company(id);
            companies.put(id, result);
        }
        return result;
    }

    private static boolean isReachable (Company x, Company y, HashSet<Company> yNotReachable)
    {
        for (Company c : x.payees)
        {
            if (c == y) {
                return true;
            }
            if (!yNotReachable.contains(c)) {
                boolean isReachable = isReachable(c, y, yNotReachable);
                if (isReachable) {
                    return true;
                }
                yNotReachable.add(c);
            }
        }
        return false;
    }

    static boolean isReachable (Company x, Company y)
    {
        java.util.HashSet yNotReachable = new HashSet();
        return isReachable(x, y, yNotReachable);
    }


    static void isFlowPossible(int x, int y, int N, int M, int[] A, int[] B)
    {
        Network network = new Network();
        for (int i=0; i<M; ++i) {
            Company ca = network.findOrCreate(A[i]);
            Company cb = network.findOrCreate(B[i]);
            ca.addPayee(cb);
        }
        Company cx = network.find(x);
        Company cy = network.find(y);
        if (cx == null || cy == null) {
            System.out.println("NO");
        }
        else {
            boolean result = isReachable(cx, cy);
            System.out.println(result ? "YES" : "NO");
        }
    }

    public static void main (String[] args)
    {
        int[] A = {1,2,3,4,1};
        int[] B = {2,3,4,5,5};
        isFlowPossible(1,3,5,5,A,B);

    }
}
