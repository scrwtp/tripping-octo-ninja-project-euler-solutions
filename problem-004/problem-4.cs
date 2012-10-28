using System;
using System.Collections.Generic;
using System.Linq;
using System.Diagnostics;

namespace scrwtp.ProjectEuler
{
    public class Program
    {
        public static void Main(string[] args)
        {
            var timer = new Stopwatch();

            var problem = new Problem4();

            timer.Start();
            var result = problem.Run(3, 3);
            timer.Stop();
            
            System.Console.WriteLine(string.Format("The result is {0}", result));
            System.Console.WriteLine(string.Format("elapsed time {1} ms, {2} ticks", result, timer.ElapsedMilliseconds, timer.ElapsedTicks));
            System.Console.ReadKey();
        }
    }

    internal class Problem4
    {
        /// <summary>
        /// Runs the algorithm for specified palindrome base length and factor length.
        /// </summary>
        /// <param name="palindromeBaseLength">Length of the palindrome base (half of the actual palindrome, 3 in the task, we don't support odd length palindroms because we're not required to).</param>
        /// <param name="factorLength">Length of the factors. (3 in the task).</param>
        /// <returns></returns>
        public int Run(int palindromeBaseLength, int factorLength)
        {
            return GeneratePalindroms(palindromeBaseLength).Where(x => IsProductOfFactorsWithLength(x, factorLength)).FirstOrDefault();
        }

        private IEnumerable<int> GeneratePalindroms(int length)
        {
            int from = (int)Math.Pow(10, length);
            int to = (int)Math.Pow(10, length - 1);

            for (int i = from - 1; i >= to; i--)
            {
                yield return (from * i + i.Invert());
            }
        }

        private bool IsProductOfFactorsWithLength(int value, int length)
        {
            var upper = Math.Pow(10, length);
            var lower = Math.Pow(10, length - 1);

            return value.GetFactorPairs().Any(x => x.Item1 >= lower && x.Item1 < upper && x.Item2 >= lower && x.Item2 < upper);
        }
    }

    /// <summary>
    /// Some methods extracted as extensions that can be useful in the future
    /// </summary>
    public static class Extensions
    {
        public static int Invert(this int value)
        {
            int acc = 0;
            int len = (int)Math.Log10(value);

            for (int i = len; i >= 0; i--)
            {
                var positionSrc = (int)Math.Pow(10, i);         // power of 10 corresponding to original decimal position of a digit
                var positionDst = (int)Math.Pow(10, len - i);   // power of 10 corresponding to decimal position of a digit in the result

                var digit = value / positionSrc;
                acc += digit * positionDst;
                value = (int)(value % positionSrc);
            }

            return acc;
        }

        public static IEnumerable<Tuple<int, int>> GetFactorPairs(this int value)
        {
            var from = (int)Math.Sqrt(value);
            var to = 2;

            for (int i = from; i >= to; i--)
            {
                if (value % i == 0)
                {
                    var factor1 = i;
                    var factor2 = value / i;

                    yield return Tuple.Create<int, int>(factor1, factor2);
                }
            }
        }
    }
}
