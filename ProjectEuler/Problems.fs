﻿module Problems

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
        |> Seq.fold (*) 1I

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