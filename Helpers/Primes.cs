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

			for (int i = 2; i < sieve.Length; i++)
			{
				for (int j = 2;; j++)
				{
					int index = i*j;
					if (index >= sieve.Length)
						break;

					sieve[index] = false;
				}
			}

			return sieve;
		}
	}
}