using System;

namespace Helpers
{
	public static class Primes
	{
		public static bool[] Sieve(int max)
		{
			bool[] sieve = new bool[max + 1];
			for (int i = 0; i < sieve.Length; i++)
				sieve[i] = true;

			sieve[0] = false;
			sieve[1] = false;

			int bound = (int)Math.Sqrt(max);
			for (int i = 2; i <= bound; i++)
			{
				if (!sieve[i])
					continue;

				for (int j = i + i; j < sieve.Length; j += i)
				{
					sieve[j] = false;
				}
			}

			return sieve;
		}

		public static int[] PrimeFactorSieve(int max)
		{
			int[] sieve = new int[max];
			sieve[0] = 0;
			sieve[1] = 1;

			int bound = (int)Math.Sqrt(max);
			for (int i = 2; i <= bound; i++)
			{
				if (sieve[i] != 0)
					continue; // Composite

				for (int j = i; j < sieve.Length; j += i)
					sieve[j]++;
			}

			return sieve;
		}
	}
}