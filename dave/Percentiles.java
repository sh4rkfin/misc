package dave;

import java.util.Arrays;
import java.util.Random;

/**
*/
public abstract class Percentiles
{
    public static abstract class RandomVariable
    {
        Random random;

        protected RandomVariable(Random random)
        {
            this.random = random;
        }

        public abstract double nextValue ();
    }

    public static class NormalRandomVariable extends RandomVariable
    {
        double mean;
        double stddev;

        public NormalRandomVariable (Random random, double mean, double stddev)
        {
            super(random);
            this.mean = mean;
            this.stddev = stddev;
        }

        @Override
        public double nextValue()
        {
            return random.nextGaussian() * stddev + mean;
        }
    }

    public static class TwoPopulationRandomVariable extends RandomVariable
    {
        RandomVariable first;
        RandomVariable second;
        double ratio;

        public TwoPopulationRandomVariable (Random random, RandomVariable first, RandomVariable second, double ratio)
        {
            super(random);
            this.first = first;
            this.second = second;
            this.ratio = ratio;
        }

        @Override
        public double nextValue()
        {
            double value = random.nextDouble();
            if (value < ratio) {
                return first.nextValue();
            }
            return second.nextValue();
        }
    }

    public static class UniformRandomVariable extends RandomVariable
    {
        double min;
        double max;
        double len;

        public UniformRandomVariable (Random random, double min, double max)
        {
            super(random);
            this.min = min;
            this.max = max;
            len = max - min;
        }

        @Override
        public double nextValue()
        {
            return random.nextDouble() * len + min;
        }
    }

    public static class Stats
    {
        private int count;
        private double sum;
        private double sumsquares;

        public Stats (int count, double sum, double sumsquares)
        {
            this.count = count;
            this.sum = sum;
            this.sumsquares = sumsquares;
        }

        public Stats ()
        {
            this(0,0,0);
        }

        public double mean ()
        {
            return sum / count;
        }

        public double variance ()
        {
            double mean = mean();
            return (sumsquares - mean * mean * count) / (count - 1);
        }

        public double scaledVariance ()
        {
            return variance() / count;
        }

        public double stddev ()
        {
            return Math.sqrt(variance());
        }

        public void add (double value)
        {
            ++count;
            sum += value;
            sumsquares += value * value;
        }

        @Override
        public String toString()
        {
            return "mean: " + mean() + ", stddev: " + stddev();
        }
    }

    public static double computeTStatistic (Stats first, Stats second)
    {
        double num = first.mean() - second.mean();
        double denom = Math.sqrt(first.scaledVariance() + second.scaledVariance());
        return num / denom;
    }

    public static double computeDegreesOfFreedom (Stats first, Stats second)
    {
        double num = Math.pow(first.scaledVariance() + second.scaledVariance(),2);
        double denom = Math.pow(first.scaledVariance(), 2) / (first.count - 1) +
                       Math.pow(second.scaledVariance(), 2) / (second.count - 1);
        return num / denom;
    }

    public static double computePercentile (
            RandomVariable random,
            int trials,
            int percentile
    )
    {
        double[] samples = new double[trials];
        for (int i=0; i<samples.length; ++i) {
            samples[i] = random.nextValue();
        }
        Arrays.sort(samples);
        int percentileIdx = (percentile * samples.length) / 100;
        return samples[percentileIdx];
    }

    public static double computeMean (double[] samples)
    {
        double sum = 0;
        for (double sample : samples) {
            sum += sample;
        }
        return sum / samples.length;
    }

    public static Stats computeStddev (double[] samples)
    {
        Stats result = new Stats();
        for (double sample : samples) {
            result.add(sample);
        }
        return result;
    }

    public static Stats computeStats (int trials, RandomVariable randomVariable)
    {
        double[] samples = new double[trials];
        Stats stats = new Stats();
        for (int i=0; i<samples.length; ++i) {
            samples[i] = randomVariable.nextValue();
            //samples[i] = computePercentile(randomVariable, 10000, 90);
            stats.add(samples[i]);
            //System.out.println("" + samples[i]);
        }
        return stats;
    }

    public static void main (String[] args)
    {
        Random random = new Random();
        RandomVariable normal = new NormalRandomVariable(random, 1.815, 1);
        RandomVariable normal2 = new NormalRandomVariable(random, 3, 2);
        RandomVariable uniform = new UniformRandomVariable(random, 0, 10* Math.sqrt(12));
        RandomVariable twopop = new TwoPopulationRandomVariable(random, normal, normal2, 0.50);
        int trials = 10000;
        Stats stats = computeStats(trials, normal);
        System.out.println("sample: " + stats + ", 3-sigma: " + 3 * stats.stddev());
        Stats second = computeStats(trials, normal2);
        System.out.println("sample: " + second + ", 3-sigma: " + 3 * second.stddev());
        System.out.println("t-statistic: " + computeTStatistic(stats, second) +
                           ", dof: " + computeDegreesOfFreedom(stats, second));
    }
}
