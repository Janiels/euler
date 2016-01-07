module Problems

open System
open System.IO

let inputFolder = Path.Combine (__SOURCE_DIRECTORY__, "Input")
#if INTERACTIVE
#r @"..\Helpers\bin\Release\Helpers.dll"
#endif

let inputPath (n : int) = Path.Combine (inputFolder, n.ToString () + ".txt")

module Permutations =
    let unordered seq =
        Helpers.Permutations.Unordered(seq)

    let ordered seq =
        Helpers.Permutations.Ordered(seq)

module Primes =
    let sieve n =
        Helpers.Primes.Sieve(n)

    let isPrime (n : int) =
        match n with
            | _ when n < 2 -> false
            | 2 -> true
            | _ when n % 2 = 0 -> false
            | _ -> 
                let root = int (sqrt (float n))
                seq {3..2..root} |> Seq.forall (fun i -> n % i <> 0)

module Triangles =
    // https://en.wikipedia.org/wiki/Tree_of_primitive_Pythagorean_triples
    let pythagoreanTriplesWithSumBelow num =
        let rec enumerate (a, b, c) =
            seq {
                if a + b + c <= num then
                    let nextScaled ((a, b, c), i) =
                        if (a + b + c) * i <= num then
                            Some((a * i, b * i, c * i), ((a, b, c), i + 1))
                        else
                            None

                    yield! Seq.unfold nextScaled ((a, b, c), 1)

                    let a1, b1, c1 = a + -2 * b + 2 * c, 2 * a + -b + 2 * c, 2 * a + -2 * b + 3 * c
                    yield! enumerate (a1, b1, c1)

                    let a2, b2, c2 = a + 2 * b + 2 * c, 2 * a + b + 2 * c, 2 * a + 2 * b + 3 * c
                    yield! enumerate (a2, b2, c2)

                    let a3, b3, c3 = -1 * a + 2 * b + 2 * c, -2 * a + b + 2 * c, -2 * a + 2 * b + 3 * c
                    yield! enumerate (a3, b3, c3)
            }

        enumerate (3, 4, 5)

module Problem1 =
    let answer =
        seq { 1..999 }
        |> Seq.filter (fun n -> n % 3 = 0 || n % 5 = 0)
        |> Seq.sum

module Problem2 =
    let fibs = Seq.unfold (fun (cur, next) -> Some(cur, (next, cur + next))) (1, 2)
    let answer =
        fibs
        |> Seq.filter (fun n -> n % 2 = 0)
        |> Seq.takeWhile (fun n -> n <= 4000000)
        |> Seq.sum

module Problem3 =
    let num = 600851475143L
    let root = int (sqrt (float num))
    let answer =
        seq {2..root}
        |> Seq.filter (fun i -> num % (int64 i) = 0L)
        |> Seq.filter Primes.isPrime
        |> Seq.max

module Problem4 =
    let isPalindrome n =
        let arr = (string n).ToCharArray()
        arr = Array.rev arr
    let threeDigits = seq { 100..999 }
    let products =
        threeDigits
        |> Seq.collect
            (fun i ->
                threeDigits
                |> Seq.map (fun n -> n * i))

    let answer =
        products
        |> Seq.filter isPalindrome
        |> Seq.max

module Problem5 =
    let isDivisible n =
        seq { 1..20 }
        |> Seq.forall (fun i -> n % i = 0)

    let answer =
        Seq.initInfinite (fun i -> 1 + i)
        |> Seq.map (fun i -> i * 20)
        |> Seq.filter isDivisible
        |> Seq.head

module Problem6 =
    let sumOfSquare =
        seq { 1..100 }
        |> Seq.map (fun i -> i * i)
        |> Seq.sum

    let squareOfSum =
        seq { 1..100 }
        |> Seq.sum
        |> (fun i -> i * i)

    let answer = squareOfSum - sumOfSquare

module Problem7 =
    let answer =
        Seq.initInfinite (fun i -> 3 + i)
        |> Seq.filter Primes.isPrime
        |> Seq.item 9999 // we don't include 2 in this list

module Problem8 =
    let nums =
        File.ReadAllLines (inputPath 8)
        |> Seq.collect id
        |> Seq.map (fun c -> int64 (string c))

    let answer =
        nums
        |> Seq.windowed 13
        |> Seq.map(fun window -> window |> Seq.fold (*) 1L)
        |> Seq.max

module Problem9 =
    let triples =
        seq {
            for a = 1 to 1000 do
                for b = a + 1 to 1000 do
                    let c = 1000 - b - a
                    yield (a, b, c)
        }

    let i, j, k =
        triples
        |> Seq.filter (fun (i,j,k) -> i * i + j * j = k * k)
        |> Seq.exactlyOne

    let answer = i * j * k

module Problem10 =
    let answer =
        seq { 2..1999999 }
        |> Seq.filter Primes.isPrime
        |> Seq.map (fun i -> (int64 i))
        |> Seq.sum

module Problem11 =
    let grid =
        File.ReadAllLines (inputPath 11)
        |> Array.map (fun line -> line.Split (' ') |> Array.map int)

    let quarts = seq {
        for y = 0 to grid.Length - 1 do
            for x = 0 to grid.[y].Length - 4 do
                yield
                    seq { 0..3 }
                    |> Seq.map (fun i -> grid.[y].[x + i])

        for y = 0 to grid.Length - 4 do
            for x = 0 to grid.[y].Length - 1 do
                yield
                    seq { 0..3 }
                    |> Seq.map (fun i -> grid.[y + i].[x])

        for y = 0 to grid.Length - 4 do
            for x = 0 to grid.[y].Length - 4 do
                yield
                    seq { 0..3 }
                    |> Seq.map (fun i -> grid.[y + i].[x + i])

        for y = 0 to grid.Length - 4 do
            for x = 3 to grid.[y].Length - 1 do
                yield
                    seq { 0..3 }
                    |> Seq.map (fun i -> grid.[y + i].[x - i])
    }

    let products =
        quarts
        |> Seq.map (fun nums -> nums |> Seq.fold (*) 1)

    let answer = products |> Seq.max

module Problem12 =
    let numDivisors n =
        if n = 1L then 1
        else
            let bound = int64 (sqrt (float n))
            (seq { 1L..bound }
            |> Seq.filter (fun i -> n % i = 0L)
            |> Seq.length) * 2

    let triangleNumbers = Seq.unfold (fun (t, i) -> Some(t, (t + i, i + 1L))) (1L, 2L)

    let answer =
        triangleNumbers
        |> Seq.find (fun i -> (numDivisors i) > 500)

module Problem13 =
    let sum =
        File.ReadAllLines (inputPath 13)
        |> Seq.map (fun line -> (bigint.Parse line))
        |> Seq.sum

    let answer = (string sum).Substring (0, 10)

module Problem14 =
    let collatz n =
        Seq.unfold
            (fun i ->
                if i = 0L then None
                elif i = 1L then Some(i, 0L)
                elif i % 2L = 0L then Some(i, i / 2L)
                else Some(i, i * 3L + 1L))
            n

    let answer = 
        seq { 1L..999999L }
        |> Seq.maxBy (fun n -> Seq.length (collatz n))

module Problem15 =
    let rec countWays =
        let dict = System.Collections.Generic.Dictionary<_,_>();
        fun curX curY ->
            match dict.TryGetValue ((curX, curY)) with
            | true, v -> v
            | false, _ ->
                let res =
                    if curX = 20 then 1L
                    elif curY = 20 then 1L
                    else (countWays (curX + 1) curY) + (countWays curX (curY + 1))
                dict.Add ((curX, curY), res)
                res

    let answer = (countWays 0 0)

module Problem16 =
    let i = 2I ** 1000
    let answer =
        (string i)
        |> Seq.map (fun c -> int (string c))
        |> Seq.sum

module Problem17 =
    let rec numInEnglish n =
        match n with
        | 1 -> "one"
        | 2 -> "two"
        | 3 -> "three"
        | 4 -> "four"
        | 5 -> "five"
        | 6 -> "six"
        | 7 -> "seven"
        | 8 -> "eight"
        | 9 -> "nine"
        | 10 -> "ten"
        | 11 -> "eleven"
        | 12 -> "twelve"
        | 13 -> "thirteen"
        | 14 -> "fourteen"
        | 15 -> "fifteen"
        | 16 -> "sixteen"
        | 17 -> "seventeen"
        | 18 -> "eighteen"
        | 19 -> "nineteen"
        | 20 -> "twenty"
        | 30 -> "thirty"
        | 40 -> "forty"
        | 50 -> "fifty"
        | 60 -> "sixty"
        | 70 -> "seventy"
        | 80 -> "eighty"
        | 90 -> "ninety"
        | 1000 -> "one thousand"
        | _ -> if n % 100 = 0 then (numInEnglish (n / 100)) + " hundred"
               elif n < 100 then (numInEnglish (n / 10 * 10)) + (numInEnglish (n % 10))
               else (numInEnglish (n / 100 * 100)) + " and " + (numInEnglish (n % 100))

    let answer =
        seq { 1..1000 }
        |> Seq.map numInEnglish
        |> Seq.map (fun s -> s.Replace (" ", ""))
        |> Seq.map (fun s -> s.Length)
        |> Seq.sum

module Problem18And67 =
    let bestRoute (treeString:string[]) =
        let tree =
            treeString
            |> Array.map (fun line -> (line.Split (' ') |> Array.map (fun s -> (int s))))

        let rec bestTo =
            let dict = System.Collections.Generic.Dictionary<_, _>()
            fun curX curY ->
                match dict.TryGetValue ((curX, curY)) with
                | true, v -> v
                | false, _ ->
                    let res =
                        let value = tree.[curY].[curX]
                        if curY = 0 then value
                        elif curX = 0 then value + (bestTo 0 (curY - 1))
                        elif curX = tree.[curY].Length - 1 then value + (bestTo (curX - 1) (curY - 1))
                        else value + (max (bestTo (curX - 1) (curY - 1)) (bestTo curX (curY - 1)))

                    dict.Add ((curX, curY), res)
                    res

        seq { 0..(Array.last tree).Length - 1 }
        |> Seq.map (fun i -> (bestTo i (tree.Length - 1)))
        |> Seq.max

    let tree18 = File.ReadAllLines (inputPath 18)
    let answer18 = bestRoute tree18

    let tree67 = File.ReadAllLines (inputPath 67)
    let answer67 = bestRoute tree67

module Problem19 =
    let firsts =
        seq { 1901..2000 }
        |> Seq.collect (fun year -> seq { 1..12 } |> Seq.map (fun month -> System.DateTime (year, month, 1)))

    let answer =
        firsts
        |> Seq.filter (fun d -> d.DayOfWeek = System.DayOfWeek.Sunday)
        |> Seq.length

module Problem20 =
    let fac n =
        seq { 1I..n }
        |> Seq.reduce (*)

    let answer =
        fac 100I
        |> string
        |> Seq.map (fun c -> int (string c))
        |> Seq.sum

module Problem21 =
    let properDivisors n =
        let bound = int (sqrt (float n))
        seq { 2..bound }
        |> Seq.filter (fun i -> n % i = 0)
        |> Seq.collect (fun i -> seq { yield i; yield n / i })
        |> Seq.append [1]
        |> Seq.distinct

    let sumOfProperDivisors n =
        properDivisors n
        |> Seq.sum

    let isAmicable n =
        let sum = sumOfProperDivisors n
        sum <> n && sumOfProperDivisors sum = n

    let answer =
        seq { 1..9999 }
        |> Seq.filter isAmicable
        |> Seq.sum

module Problem22 =
    let names = File.ReadAllText(inputPath 22).Replace("\"", "").Split(',')
    let namesSorted =
        names
        |> Seq.sort

    let nameScore index name =
        let nameSum =
            name
            |> Seq.map (fun c -> (int c) - (int 'A'B) + 1)
            |> Seq.sum

        nameSum * (index + 1)

    let answer =
        namesSorted
        |> Seq.mapi nameScore
        |> Seq.sum

module Problem23 =
    open Problem21

    let isAbundant n =
        (sumOfProperDivisors n) > n

    let abundantNumbersBelow28124 =
        seq { 1..28123 }
        |> Seq.filter isAbundant
        |> Set.ofSeq

    /// Only works for n < 28124
    let isSumOfTwoAbundant n =
        abundantNumbersBelow28124
        |> Seq.takeWhile (fun an -> an < n)
        |> Seq.exists (fun an -> abundantNumbersBelow28124 |> Set.contains (n - an))

    let answer =
        seq { 1..28123 }
        |> Seq.filter (fun i -> not (isSumOfTwoAbundant i))
        |> Seq.sum

module Problem24 =
    let millionth =
        Permutations.ordered [|0..9|]
        |> Seq.item 999999

    let answer =
        System.String.Join("", millionth)

module Problem25 =
    let fibs =
        Seq.unfold (fun (prev, cur) -> Some(cur, (cur, prev + cur))) (0I, 1I)

    let answer =
        fibs
        |> Seq.findIndex (fun n -> (String.length (string n)) = 1000)
        |> (+) 1

module Problem26 =
    let periodLength n =
        let rec periodLengthWithHistory rest (history : Map<int, int>) index =
            match history.TryFind rest with
            | Some i -> index - i
            | None ->
                let nextRest = rest * 10 % n
                if nextRest = 0 then
                    0
                else
                    periodLengthWithHistory nextRest (history.Add(rest, index)) (index + 1)

        periodLengthWithHistory 1 Map.empty 0

    let answer =
        seq { 1..999 }
        |> Seq.maxBy periodLength

module Problem27 =
    let primes = Primes.sieve 100000

    let isPrime n =
        if n < 0 then
            false
        else
            primes.[n]

    let numPrimesProduced func =
        Seq.initInfinite id
        |> Seq.map func
        |> Seq.takeWhile isPrime
        |> Seq.length

    let quadratic a b n = n * n + a * n + b

    let a, b =
        seq { -999..999 }
        |> Seq.collect (fun i -> seq { -999..999 } |> Seq.map (fun j -> (i, j)))
        |> Seq.maxBy (fun (i, j) -> numPrimesProduced (quadratic i j))

    let answer = a * b

module Problem28 =
    // It can be seen that each diagonal grows by an additional 8 each time. For example
    // the right lower corner has values 1, 3, 13, 31, 57 which are differences of
    // 2, 10, 18, 26. The left lower corner has 1, 5, 17, 37 = 4, 12, 20

    // 21 22 23 24 25
    // 20  7  8  9 10
    // 19  6  1  2 11
    // 18  5  4  3 12
    // 17 16 15 14 13
    let diagonals start =
        Seq.unfold (fun (cur, growth) -> Some(cur, (cur + growth, growth + 8))) (start, 8 + start - 1)

    let diagonalSum start amount =
        diagonals start
        |> Seq.take amount
        |> Seq.sum

    let answer =
        (diagonalSum 3 500) +
        (diagonalSum 5 500) +
        (diagonalSum 7 500) +
        (diagonalSum 9 500) +
        1

module Problem29 =
    let answer =
        seq { 2I..100I }
        |> Seq.collect (fun a -> seq { 2..100 } |> Seq.map (fun b -> (a, b)))
        |> Seq.map (fun (a, b) -> pown a b)
        |> Seq.distinct
        |> Seq.length

module Problem30 =
    let isSumOfFifthPowers n =
        let digitFifthPowerSum =
            string n
            |> Seq.map (fun c -> int (string c))
            |> Seq.map (fun i -> pown i 5)
            |> Seq.reduce (+)

        digitFifthPowerSum = n

    // The max sum of a 6 digit number is 9^5 * 6 = 354294
    // This grows slower than the numbers themselves, so we can stop
    // there
    let nums =
        { 10..354294 }
        |> Seq.filter isSumOfFifthPowers

    let answer = nums |> Seq.sum

module Problem31 =
    let coins = [1; 2; 5; 10; 20; 50; 100; 200]

    let waysToMake amount =
        let rec waysToMakeWithMax amount maxCoin =
            if amount < 0 then
                0
            elif amount = 0 then
                1
            else
                coins
                |> Seq.filter (fun coin -> coin <= maxCoin)
                |> Seq.map (fun coin -> waysToMakeWithMax (amount - coin) coin)
                |> Seq.sum

        waysToMakeWithMax amount (coins |> Seq.max)

    let answer = waysToMake 200

module Problem32 =
    let permutations = Permutations.unordered [|1..9|]
    let pandigital = seq {
        for num in permutations |> Seq.map (fun s -> Seq.toArray s) do
            let intSliced (arr : int[]) =
                int (System.String.Join("", arr))

            for i = 0 to 6 do
                for j = i + 1 to 7 do
                    let multiplicand = intSliced num.[..i]
                    let multiplier = intSliced num.[(i + 1)..j]
                    let product = intSliced num.[(j + 1)..]

                    if multiplicand * multiplier = product then
                        yield product
    }  

    let answer =
        pandigital
        |> Seq.distinct
        |> Seq.sum

module Problem33 =
    let isCuriousFraction (num, denom) =
        let numChars num c =
            string num
            |> Seq.filter (fun strC -> strC = c)
            |> Seq.length

        let common =
            seq { '1'..'9' }
            |> Seq.filter (fun c -> (numChars num c) = 1 && (numChars denom c) = 1)
            |> Seq.tryHead

        match common with
            | Some c ->
                let cancel n =
                    let replaced = (string n).Replace((string c), "")
                    match replaced with
                        | "" -> 0
                        | _ -> int replaced

                let cnum, cdenom = (cancel num), (cancel denom)

                if cdenom = 0 then
                    false
                else
                    (decimal cnum) / (decimal cdenom) = (decimal num) / (decimal denom)
            | None -> false

    let twoDigitSubOneFracs = seq {
        for i = 10 to 98 do
            for j = i + 1 to 99 do
                yield (i, j)
    }

    let productNum, productDenom =
        twoDigitSubOneFracs
        |> Seq.filter isCuriousFraction
        |> Seq.reduce (fun (num, denom) (i, j) -> (num * i, denom * j))

    let rec gcd x y =
        if x < y then
            gcd y x
        elif y = 0 then
            x
        else
            gcd y (x % y)

    let answer = productDenom / (gcd productNum productDenom)

module Problem34 =
    // Max value of a number with n digits:
    // 10^n - 1
    // Max value of sum of digit factorials:
    // 9! * n
    // 10^n - 1 grows much faster than 9! * n.
    // 9! * 6 = 2177280 means this is an upper bound we need to check
    // since 7 digits = 10 000 000

    let fac n =
        if n = 0 then
            1
        else
            seq { 1..n } |> Seq.reduce (*)

    let digitFacSum n =
        string n
        |> Seq.map (fun c -> fac (int (string c)))
        |> Seq.sum

    let answer =
        seq { 3..2177280 }
        |> Seq.filter (fun i -> i = digitFacSum i)
        |> Seq.sum

module Problem35 =
    let sieve = Primes.sieve 1000000

    let isCircularPrime n =
        let rotate (str : string) =
            str.[1..] + str.[0..0]

        let rec isCircularPrime n left =
            if left = 0 then
                true
            else
                sieve.[int n] && isCircularPrime (rotate n) (left - 1)
                
        isCircularPrime (n.ToString()) (n.ToString().Length)

    let answer =
        seq { 2..999999 }
        |> Seq.filter isCircularPrime
        |> Seq.length

module Problem36 =
    let rec bin n =
        match n with
        | 0 | 1 -> string n
        | _ -> (bin (n / 2)) + (string (n % 2))

    let isStringPalindrome (s : string) =
        let chars = s.ToCharArray()
        chars = Array.rev chars

    let isBinPalindrome n =
        isStringPalindrome (bin n)

    let isDecPalindrome n =
        isStringPalindrome (string n)

    let answer =
        seq { 1..999999 }
        |> Seq.filter (fun i -> isBinPalindrome i && isDecPalindrome i)
        |> Seq.sum

module Problem37 =
    let sieve = Primes.sieve 10000000

    let isTruncatablePrime n =
        let rec isTruncatableInDir shift n =
            if not sieve.[n] then
                false
            elif n < 10 then
                true
            else
                isTruncatableInDir shift (shift n)

        let rtl = isTruncatableInDir (fun n -> n / 10)
        let ltr = isTruncatableInDir (fun n -> int (n.ToString().[1..]))

        rtl n && ltr n

    let answer =
        seq { 1..9999999 }
        |> Seq.skipWhile (fun n -> n <= 7)
        |> Seq.filter isTruncatablePrime
        |> Seq.take 11
        |> Seq.sum

module Problem38 =
    let isPandigital n =
        string n
        |> Seq.groupBy id
        |> Seq.forall (fun (c, chars) -> chars |> Seq.length = 1)
        && (string n).Length = 9

    let pandigitalsDecreasing =
        Permutations.ordered [| 9..-1..1 |]
        |> Seq.map (fun n -> int (System.String.Join("", n)))

    let isConcatenatedProduct n =
        let rec isConcatenatedProductWithSeed seed mul numberString =
            match numberString with
                | "" -> true
                | _ ->
                    let next = string (seed * mul)
                    if numberString.StartsWith(next) then
                        isConcatenatedProductWithSeed seed (mul + 1) (numberString.Substring(next.Length))
                    else
                        false

        let numberString = string n
        { 1.. (numberString.Length - 1) }
        |> Seq.exists (fun len -> isConcatenatedProductWithSeed (int (numberString.Remove(len))) 1 numberString)

    let answer =
        pandigitalsDecreasing
        |> Seq.find isConcatenatedProduct

module Problem39 =
    let triangles = Triangles.pythagoreanTriplesWithSumBelow 1001

    let answer =
        triangles
        |> Seq.groupBy (fun (a, b, c) -> a + b + c)
        |> Seq.maxBy (fun (key, seq) -> seq |> Seq.length)
        |> fst

module Problem40 =
    let digitSeq =
        Seq.initInfinite (fun i -> 1 + i)
        |> Seq.collect (fun i -> string i)
        |> Seq.map int

    let answer =
        [1; 10; 100; 1000; 10000; 100000; 1000000]
        |> Seq.map (fun i -> digitSeq |> Seq.item (i - 1))
        |> Seq.reduce (*)

module Problem41 =
    let pandigitalsDecreasing =
        { 9..-1..1 }
        |> Seq.collect (fun i -> Permutations.ordered [|i .. -1 .. 1|] |> Seq.map (fun perm -> int (System.String.Join("", perm))))

    let answer =
        pandigitalsDecreasing
        |> Seq.filter Primes.isPrime
        |> Seq.head

module Problem42 =
    let isTriangleNumber n =
        let index = -1.0/2.0 + (sqrt (1.0/4.0 + 2.0 * (float n)))
        index = (float (int index))

    let words = File.ReadAllText(inputPath 42).Replace("\"", "").Split(',')

    let isTriangleWord word =
        let wordSum =
            word
            |> Seq.map (fun c -> (int c) - (int 'A') + 1)
            |> Seq.sum

        isTriangleNumber wordSum

    let answer =
        words
        |> Seq.filter isTriangleWord
        |> Seq.length

module Problem43 =
    let pandigitals =
        Permutations.unordered [|0..9|]
        |> Seq.filter (fun num -> num.[0] <> 0)
        |> Seq.map (fun i -> int64 (System.String.Join("", i)))

    let hasProperty n =
        string n
        |> Seq.map (fun c -> int (string c))
        |> Seq.skip 1
        |> Seq.windowed 3
        |> Seq.zip [2;3;5;7;11;13;17]
        |> Seq.forall (fun (prime, window) -> (int (System.String.Join("", window))) % prime = 0)

    let answer =
        pandigitals
        |> Seq.filter hasProperty
        |> Seq.sum

module Problem44 =
    let pentagonal i =
        i * (3L * i - 1L) / 2L

    let pentagonals =
        Seq.unfold (fun i -> Some(pentagonal i, i + 1L)) 1L

    let isPentagonal n =
        if n <= 0L then
            false
        else
            let index = int64 (System.Math.Round(1.0/6.0 + sqrt(1.0/36.0 + 2.0/3.0 * (float n))))
            pentagonal index = n || pentagonal (index + 1L) = n || pentagonal (index - 1L) = n

    let pentagonalsWhereSumCouldBePentagonal otherPentagonal =
        Seq.unfold (fun i -> Some(pentagonal i, i + 1L)) 1L
        |> Seq.takeWhile (fun i -> pentagonal i + otherPentagonal >= pentagonal (i + 1L))
        |> Seq.map pentagonal

    let answer =
        pentagonals
        |> Seq.collect (fun a -> pentagonalsWhereSumCouldBePentagonal a |> Seq.map (fun b -> a, b))
        |> Seq.filter (fun (a, b) -> isPentagonal b)
        |> Seq.filter (fun (a, b) -> isPentagonal (a + b))
        |> Seq.head

module Problem45 =
    let isHexagonal n =
        let index = int64 (System.Math.Round(1.0/4.0 + sqrt(1.0 + 8.0* (float n)) / 4.0))
        (index * (2L * index - 1L)) = n

    let triangularNumbers = Seq.unfold (fun n -> Some(n*(n+1L)/2L, n + 1L)) 1L

    let answer =
        triangularNumbers
        |> Seq.skipWhile (fun i -> i <= 40755L)
        |> Seq.find (fun n -> (isHexagonal n) && (Problem44.isPentagonal n))

module Problem46 =
    let sieve = Primes.sieve 10000000
    let isPrime n =
        if n < 0 then
            false
        else
            sieve.[n]

    let squaresBelow n = seq {
        let mutable i = 1
        while i * i < n do
            yield i * i
            i <- i + 1
        }

    let answer =
        Seq.unfold (fun cur -> Some(cur + 2, cur + 2)) 3
        |> Seq.filter (fun num -> not (sieve.[num]))
        |> Seq.find (fun num -> squaresBelow num |> Seq.forall (fun square -> (not << isPrime) (num - 2 * square)))

module Problem48 =
    let sum =
        seq { 1..1000 }
        |> Seq.map (fun i -> pown (bigint i) i)
        |> Seq.sum

    let lastN n seq =
        seq
        |> Seq.skip (Seq.length seq - n)

    let answer =
        sum
        |> string
        |> lastN 10
        |> (fun r -> ("", r))
        |> System.String.Join

module Problem49 =
    let sieve = Primes.sieve 9999

    let findConsecutiveEqDiff (array : int[]) =
        let diffs =
            array
            |> Seq.mapi (fun i v -> { i + 1..array.Length - 1} |> Seq.map (fun i2 -> (array.[i2] - v)))
            |> Seq.collect id
            |> Seq.distinct

        array
        |> Seq.collect (fun v -> diffs |> Seq.map (fun d -> (v, v + d, v + 2 * d)))
        |> Seq.filter (fun (a, b, c) -> array |> Array.contains b && array |> Array.contains c)

    let answer =
        sieve
        |> Seq.mapi (fun n isPrime -> n, isPrime)
        |> Seq.filter (fun (n, isPrime) -> isPrime)
        |> Seq.map (fun (n, _) -> string n |> Permutations.unordered)
        |> Seq.map (fun permuts -> permuts |> Seq.map (fun permut -> int (System.String.Join("", permut))))
        |> Seq.map (fun permuts -> permuts |> Seq.filter (fun n -> sieve.[n]) |> Seq.distinct |> Seq.sort |> Seq.toArray)
        |> Seq.filter (fun permuts -> permuts.Length >= 3 && permuts.[0] >= 1000)
        |> Seq.collect findConsecutiveEqDiff
        |> Seq.distinct

module Problem50 =
    let sieve = Primes.sieve 1000000

    let cumSums =
        sieve
        |> Seq.mapi (fun i isPrime -> i, isPrime)
        |> Seq.filter (fun (_, isPrime) -> isPrime)
        |> Seq.map (fun (i, _) -> i)
        |> Seq.scan (+) 0
        |> Seq.toArray

    let bestStartingFrom i =
        let highestIndex =
            {i..cumSums.Length - 1}
            |> Seq.takeWhile (fun j -> cumSums.[j] - cumSums.[i] < 1000000)
            |> Seq.filter (fun j -> sieve.[cumSums.[j] - cumSums.[i]])
            |> Seq.append [i]
            |> Seq.max

        (highestIndex - i, cumSums.[highestIndex] - cumSums.[i])

    let (consecutive, answer) =
        {0..cumSums.Length - 1}
        |> Seq.map bestStartingFrom
        |> Seq.maxBy (fun (num, prime) -> num)

module Problem52 =
    let areSameDigits n1 n2 =
        string n1
        |> Seq.sort
        |> Seq.compareWith Operators.compare (string n2 |> Seq.sort)
        |> (=) 0

    let answer =
        Seq.initInfinite (fun i -> 1 + i)
        |> Seq.find (fun i -> {2..6} |> Seq.forall (fun mul -> areSameDigits i (i * mul)))

module Problem53 =
    let nChooseR n r =
        let fac n =
            if n = 0I then
                1I
            else
                seq {1I..n} |> Seq.reduce (*)

        (fac n) / ((fac r) * (fac (n - r)))

    let answer =
        seq {1I..100I}
        |> Seq.collect (fun n -> seq {1I..n} |> Seq.map (fun r -> n, r))
        |> Seq.map (fun (n, r) -> nChooseR n r)
        |> Seq.filter (fun amount -> amount > 1000000I)
        |> Seq.length

module Problem55 =
    let isLychrelNumber (n : bigint) =
        let isPalindrome (n : bigint) =
            let chars = (string n) |> Seq.toArray
            chars = Array.rev chars

        let rec isLychrelNumberInIts (n : bigint) its =
            let reversed =
                string n
                |> Seq.rev
                |> Seq.map string
                |> Seq.reduce (+)

            let next = n + (bigint.Parse reversed)
            if isPalindrome next then
                false
            elif its = 1 then
                true
            else
                isLychrelNumberInIts next (its - 1)

        isLychrelNumberInIts n 50

    let answer =
        seq {1I..9999I}
        |> Seq.filter isLychrelNumber
        |> Seq.length

module Problem56 =
    let digitalSum (a : bigint) b =
        let result = pown a b
        string result
        |> Seq.map (fun c -> (int c) - (int '0'))
        |> Seq.sum

    let answer =
        {1I..99I}
        |> Seq.map (fun a -> {1..99} |> Seq.map (digitalSum a) |> Seq.max)
        |> Seq.max

module Problem75 =
    let answer =
        Triangles.pythagoreanTriplesWithSumBelow 1500000
        |> Seq.groupBy (fun (a, b, c) -> a + b + c)
        |> Seq.filter (fun (sum, triples) -> (triples |> Seq.length) = 1)
        |> Seq.length