module Problems

open System.IO

#if !INTERACTIVE
let inputFolder = Path.Combine (__SOURCE_DIRECTORY__, "Input")
#endif

let inputPath (n : int) = Path.Combine (inputFolder, n.ToString () + ".txt")

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
    let isPrime (n : int) =
        if n % 2 = 0 then false
        else
            let root = int (sqrt (float n))
            seq {3..2..root} |> Seq.forall (fun i -> n % i <> 0)

    let num = 600851475143L
    let root = int (sqrt (float num))
    let answer =
        seq {2..root}
        |> Seq.filter (fun i -> num % (int64 i) = 0L)
        |> Seq.filter isPrime
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
    let isPrime n =
        if n % 2 = 0 then false
        else
            let root = int (sqrt (float n))
            seq {3..2..root} |> Seq.forall (fun i -> n % i <> 0)

    let answer =
        Seq.initInfinite (fun i -> 3 + i)
        |> Seq.filter isPrime
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
    let isPrime n =
        if n = 2 then true
        elif n % 2 = 0 then false
        else
            let root = int (sqrt (float n))
            seq {3..2..root} |> Seq.forall (fun i -> n % i <> 0)

    let answer =
        seq { 2..1999999 }
        |> Seq.filter isPrime
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
    let rec permute (arr : 'a[]) =
        if arr.Length = 1 then
            Seq.singleton (Seq.ofArray arr)
        else
            seq {
                for i = 0 to arr.Length - 1 do
                    let without =
                        if i = 0 then
                            arr.[i+1..]
                        else
                            Array.append arr.[..i-1] arr.[i+1..]

                    for subSeq in (permute without) do
                        yield Seq.append (Seq.singleton arr.[i]) subSeq
            } 

    let millionth =
        permute [|0..9|]
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
    let sieve (n:int) =
        let arr = (Array.init n (fun i -> true))
        arr.[0] <- false
        arr.[1] <- false
        for i = 2 to n do
            let mutable j = 2
            while i * j < n do
                arr.[i * j] <- false
                j <- j + 1

        arr

    let primes = sieve 100000

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

module Problem75 =
    // https://en.wikipedia.org/wiki/Tree_of_primitive_Pythagorean_triples
    let pythagoreanTriplesBelow num =
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

    let answer =
        pythagoreanTriplesBelow 1500000
        |> Seq.groupBy (fun (a, b, c) -> a + b + c)
        |> Seq.filter (fun (sum, triples) -> (triples |> Seq.length) = 1)
        |> Seq.length