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
	}
}