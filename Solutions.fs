module Solutions

open System
open Helpers
open System.Numerics

let problem1 limit =
    printfn "%A" 1
    [1..limit - 1] |> List.filter(fun x -> x % 3 = 0 || x % 5 = 0) |> List.sum

let problem2 limit =
    fibSeq |> Seq.takeWhile(fun x -> x < limit) |> Seq.filter(fun x -> x % 2 = 0) |> Seq.sum

let problem3 number =
    primeFactorsLong number |> set |> Seq.max

let problem4 limit =
    let values = 
        [for x in 1..limit do
         for y in 1..limit do
         yield x * y]
    values |> List.filter isPalindrome |> List.max

let problem5 limit =
    [1L..limit] |> List.reduce lcm

let problem6 limit =
    let sumOfSquares = [1..limit] |> List.map square |> List.sum
    let squareOfSum = [1..limit] |> List.sum |> square
    squareOfSum - sumOfSquares

let problem7 n =
    naturalNumbersFrom 2L |> Seq.filter isPrime |> Seq.skip (n-1) |> Seq.take 1 |> Seq.find (fun _ -> true)

let problem8 (a:string) b =
    let x = a.ToCharArray() |> Array.map (fun y -> bigint (int (y.ToString())))
    let mult a b = [0 .. Array.length a - b] |> List.map (fun c -> a.[c..c+b-1] |> Array.reduce (fun (x:bigint) y -> x * y))
    mult x b |> List.max

let problem9 sum =
    let (a, b, c) = pythagoreanTriplets sum |> Seq.find(fun (a, b, c) -> square a + square b = square c)
    a * b * c

let problem10 limit =
    2L + ([3L .. 2L .. limit] |> Seq.filter isPrime |> Seq.sum)

let problem11 (xs:string[]) y =
    let a = xs |> Array.map (fun x -> x.Split(' ') |> Array.map int)

    let maxProduct y x =
        x |> Seq.windowed y |> Seq.map product |> Seq.max

    let rowsMax = a |> Array.map (maxProduct y) |> Seq.max
    let columnsMax = [0 .. ((a.[0]) |> Array.length) - 1] |> List.map (fun x -> a |> Array.map (fun y -> y.[x])) |> List.map (maxProduct y) |> Seq.max

    let rightDiagonalsMax = rightDiagonals a |> Seq.map(fun x -> List.ofSeq x) |> Seq.filter (fun x -> x.Length >= y) |> Seq.map (maxProduct y) |> Seq.max
    let leftDiagonalsMax = leftDiagonals a |> Seq.map(fun x -> List.ofSeq x) |> Seq.filter (fun x -> x.Length >= y) |> Seq.map (maxProduct y) |> Seq.max

    max (max rowsMax columnsMax) (max rightDiagonalsMax leftDiagonalsMax)

let problem12 numOfDivisors =
    triangularNumbers |> Seq.map (fun x -> (x, numOfDistinctFactors x)) |> Seq.find (fun (_, n) -> n > numOfDivisors)

let problem13 (xs:string[]) =
    xs |> Array.map (fun x -> BigInteger.Parse(x)) |> Array.sum

let problem14 min max =
    [min .. max] |> List.map (fun x -> (x, collatzSequence x |> Seq.length)) |> List.maxBy (fun (_, l) -> l)

let problem15 gridSize =
    let top = [1I .. gridSize * 2I] |> bigProduct
    let bottom = [1I .. gridSize] |> bigProduct
    top / (bottom * bottom)

let problem16 power =
    let x = pown 2I power
    x.ToString().ToCharArray() |> Array.map (fun x -> int (x.ToString())) |> Array.sum

let problem17 (lines:string[]) limit =
    let dict = lines |> Array.map (fun l -> l.Split " " |> Array.map int) |> Array.map (fun a -> (a.[0], a.[1])) |> Map.ofArray
    [1 .. limit] |> List.map (countLettersInNumber dict) |> List.sum

let problem18 (data:string[]) =
    let mutable goodData = data |> Array.map (fun x -> x.Split " " |> Array.map int)
    for i in [goodData.Length - 2 .. -1 .. 0] do
        for j in [0 .. goodData.[i].Length - 1] do
            goodData.[i].[j] <- max (goodData.[i].[j] + goodData.[i + 1].[j]) (goodData.[i].[j] + goodData.[i + 1].[j + 1])
    goodData.[0].[0]

let problem19 (s:DateTime) (e:DateTime) =
    let firstDaysOfMonths = Seq.unfold(fun (d:DateTime) -> Some(d, d.AddMonths(1))) s
    firstDaysOfMonths |> Seq.takeWhile(fun x -> x <= e) |> Seq.filter(fun x -> x.DayOfWeek = DayOfWeek.Sunday) |> Seq.length

let problem20 n =
    factorial n |> digits |> Array.sum

let problem21 limit =
    [1 .. limit] |> List.map (fun x -> (x, properDivisors x |> List.sum)) |> List.filter (fun (a, b) -> a <> b && areAmicable a b) |> List.sumBy (fun (x, _) -> x)

let problem22 (names:string[]) =
    let parsedNames = parseQuotedNames names
    parsedNames |> Array.mapi (fun i n -> (n |> Seq.map (fun x -> letterValues.[x]) |> Seq.sum) * (i + 1)) |> Array.fold (fun acc el -> acc + bigint el) 0I

// this is very slow (second line 7 seconds, third one 13 seconds)
let problem23 limit =
    let abundantNumbers = [12 .. limit] |> List.filter isAbundant
    let abundantNumberSums = [ for a in abundantNumbers do yield! [ for b in abundantNumbers do yield a + b ]] |> List.distinct
    [1 .. limit] |> List.filter (fun x -> not (List.contains x abundantNumberSums)) |> List.sum

let problem24 (x:int list) n =
    let rec f acc (a:int list) n =
        let x = smallFactorial (a.Length - 1)
        let i = n / x
        let r = a.[i]
        let rs = a |> List.except [a.[i]]
        let t = n - i * x
        if rs.Length = 0 then
            (r :: acc, rs, t)
        else
            f (r :: acc) rs t
    let t, _, _ = f [] x (n - 1)
    t |> List.rev

let problem25 n =
    (largeFib |> Seq.takeWhile (fun x -> x < (BigInteger.Pow(10I, n - 1)) ) |> Seq.length) + 1

let problem26 limit =
    dfcl limit

// should we incorporate maxA somehow?
// also, it takes 1500 ms, maybe it can go faster?
let problem27 maxA maxB =
    let bs = primesUpTo maxB
    let abs = bs |> List.map (fun x -> [(-x) .. x] |> List.map (fun y -> (x, y)) ) |> List.concat
    let generator a b =
        seq {
            for n in [1 .. b-1] do
                let x = ((square n) + a * n + b)
                yield x
        }
    let seqs = abs |> List.map (fun (b, a) -> (a, b, generator a b |> Seq.takeWhile (fun x -> isPrime (int64 x)) |> Seq.length))
    let (a, b, _) = seqs |> Seq.maxBy (fun (_, _, l) -> l)
    a * b

// error checking ommited, but size needs to be odd
let problem28 size =
    let n = (size - 1) / 2
    (((16 * n * n * n) + (30 * n * n) + (26 * n)) / 3) + 1

let problem29 maxA maxB =
    [for a in 2I .. maxA do yield! [for b in 2 .. maxB do yield pown a b]] |> List.distinct |> List.length

let problem30 () =
    [2 .. (6 * pown 9 5)] |> List.filter (fun x -> x = (fifthPowersOfDigitsSum x)) |> List.sum

let problem31 total =
    coinCount total coins.Length

let problem32 () =
    [for a in 2 .. 99 do yield! [for b in 101 .. 9999 do yield (a * b, (string a) + (string b) + (string (a * b)))]] |> List.filter (fun (_, x) -> isPandigital x) |> List.distinctBy (fun (x, _) -> x) |> List.sumBy (fun (x, _) -> x)

let problem33 () =
    let f n =
        let x = n % 10
        let y = n / 100
        let z = (n / 10) % 10
        let a1 = (10 * x) + y
        let a2 = x
        let b1 = (10 * y) + z
        let b2 = z
        x > 0 && y > 0 && z > 0 && x <> y && (a1 * b2) = (a2 * b1), a2, b2

    let t = [111 .. 1000] |> List.map f |> List.filter (fun (x, _, _) -> x) |> List.map (fun (_, y, z) -> (y, z)) |> List.reduce (fun a x -> (fst a * fst x), (snd a * snd x))
    snd t / fst t

let problem34 () =
    [3 .. (6 * smallFactorial 9)] |> List.filter (fun x -> x = (digitFactorialsSum x)) |> List.sum

let problem35 limit =
    let rotations x = Seq.unfold (fun (s: int * bool) -> if (snd s && (fst s) = x) then None else Some(fst s, (rotate (fst s), true))) (x, false)
    let primes = sieve limit |> List.filter (fun x -> not (Array.contains 0 (x |> digits)))
    let primesSet = Set primes
    primes |> List.map (fun p -> rotations p |> List.ofSeq) |> List.filter (fun x -> x |> List.forall (fun y -> Set.contains y primesSet)) |> List.concat |> List.distinct |> List.length

let problem36 limit =
    let isBinaryPalindromic = isPalindromeInBase 2
    [1 .. limit] |> List.filter (fun x -> isPalindrome x && isBinaryPalindromic x) |> List.sum

// this is not really all that functional
// TODO: think about better algorithm
let problem37 () =
    let rightTruncs x = Seq.unfold (fun s -> if s > 0L then Some(s, s / 10L) else None) x

    let round size lastRoundResults =
        let a = lastRoundResults |> List.map (fun p -> [1L .. 10L] |> List.map (fun a -> (a * size) + p) |> List.filter isPrime) |> List.concat

        let r = a |> List.filter (fun x -> rightTruncs x |> Seq.forall isPrime)

        a, r

    let mutable bothTrunc = []
    let mutable lastRoundResults = [3L; 7L]
    let mutable size = 1L

    while bothTrunc.Length < 11 do
        size <- size * 10L

        let candidates, results = round size lastRoundResults

        lastRoundResults <- candidates

        bothTrunc <- bothTrunc @ results

    bothTrunc |> List.sum

// still not very functional
// TODO: work on the algorithm some more
let problem38 () =
    let mutable i = 1
    let mutable s = ""
    let pandigitals =
        [for size in [10; 100; 1000; 10000] do
            let start = (size / 10) * 9
            for n in start .. size do
                i <- 1
                s <- ""
                while s.Length < 9 do
                    s <- s + (n * i).ToString()
                    i <- i + 1
                if isPandigital s then yield s]
    pandigitals |> List.sort |> List.last

let problem39 limit =
    let l = [for p in 3 .. limit do
                for a in 1 .. (p / 3) + 2 do
                    for b in a .. p / 2 do
                        if (a * a) + (b * b) = ((p - b - a) * (p - b - a)) then yield p]
    l |> List.groupBy (fun x -> x) |> List.maxBy (fun (k, l) -> l.Length)

let problem40 () =
    let champernowne = seq {for i in 1 .. 500000 do yield! digits i}
    (champernowne |> Seq.head) * (champernowne |> Seq.skip 9 |> Seq.head) * (champernowne |> Seq.skip 99 |> Seq.head) * (champernowne |> Seq.skip 999 |> Seq.head) * (champernowne |> Seq.skip 9999 |> Seq.head) * (champernowne |> Seq.skip 99999 |> Seq.head) * (champernowne |> Seq.skip 999999 |> Seq.head)

let problem41 () =
    (sieve 7654321) |> List.sortDescending |> List.find isShortPandigital

let problem42 (w:string[]) =
    let words = parseQuotedNames w
    let numbers = words |> Array.map (fun n -> (n |> Seq.map (fun x -> letterValues.[x]) |> Seq.sum))
    let maxNumber = numbers |> Array.max
    let triangulars = triangularNumbers |> Seq.takeWhile (fun x -> x <= maxNumber) |> Set.ofSeq
    numbers |> Array.filter (fun x -> triangulars.Contains x) |> Array.length

let problem43 () =
    let noRecurringDigits x = (digits x |> Array.distinct |> Array.length) = (digits x |> Array.length)
    let missingDigit x = (Set [0; 1; 2; 3; 4; 5; 6; 7; 8; 9] - (digits x |> Set.ofArray)) |> Set.toList |> List.map (fun x -> bigint x) |> List.head
    let f x a =
        let t = a |> List.map (fun x -> x / 10I) |> Set.ofList
        [x .. x .. 987I] |> List.filter noRecurringDigits |> List.filter (fun y -> Set.contains (y % 100I) t)
    let h a b s =
        a |> List.map (fun x -> b |> List.filter (fun y -> (y % 100I) = (x / s)) |> List.map (fun z -> x % s + z * s)) |> List.concat |> List.filter noRecurringDigits
    let g p q a b =
        let c = f p a
        let d = h b c q
        c, d

    let d8d9d10 = [17I .. 17I .. 987I] |> List.filter noRecurringDigits
    let _, r = [(13I, 10I); (11I, 100I); (7I, 1000I); (5I, 10000I); (3I, 100000I); (2I, 1000000I)] |> List.fold (fun s (x, y) -> g x y (fst s) (snd s)) (d8d9d10, d8d9d10)

    r |> List.filter (hasXDigits 9) |> List.map (fun x -> (missingDigit x) * 1000000000I + x) |> List.sum

let problem44 () =
    let limit = 10000
    let pentagonalArray = pentagonalNumbers |> Seq.take limit |> Array.ofSeq
    let x = seq { for i in 0 .. limit - 1 do
                  for j in 0 .. i do
                      if isPentagonal (float (pentagonalArray.[i] - pentagonalArray.[j])) then
                          if isPentagonal (float(pentagonalArray.[i] + pentagonalArray.[j])) then
                              yield pentagonalArray.[i] - pentagonalArray.[j] }
    x |> Seq.take 1 |> Seq.head

let problem45 start =
    hexagonalNumbers |> Seq.skip start |> Seq.find (fun x -> isPentagonal (float x))

let problem46 () =
    let odds = Seq.unfold (fun s -> Some(s, s + 2)) 9
    let f x =
        let primes = sieve x
        primes |> List.fold (fun a y -> a || isInt (sqrt ((float (x - y)) / 2.0))) false
    odds |> Seq.filter (fun x -> not (f x)) |> Seq.take 1 |> Seq.head

let problem47 n =
    let ints = Seq.unfold (fun s -> Some(s, s + 1)) 1
    let windowed = Seq.windowed n ints
    windowed |> Seq.find (fun x -> x |> Array.forall (fun t -> numOfDistinctPrimeFactors t = n))

let problem48 n =
    ([1I .. n] |> List.reduce (fun a x -> a + pown x (int x))) % pown 10I 10

let problem49 () =
    // faster than set comparison
    let sortedDigits x = digits x |> Array.sort
    // remove the sequence from the example
    let primes = sieve 10000 |> List.filter (fun x -> x >= 1000 && x <> 1487 && x <> 4817 && x <> 8147) |> Set.ofList
    [for p in primes do
        for j in 2 .. 2 .. (10000 - p) / 2 do
            let q = p + j
            let r = q + j
            if Set.contains q primes && Set.contains r primes then
                if sortedDigits p = sortedDigits q && sortedDigits q = sortedDigits r then
                    yield p, q, r] |> List.head

let problem50 limit =
    let primes = sieve limit |> List.sort |> Array.ofList
    let primeSet = primes |> Set.ofArray
    let partialSums (arr:int[]) =
        let mutable sum = 0
        let mutable i = 0
        while i < arr.Length && sum < limit do
            let x = arr.[i]
            sum <- sum + x
            arr.[i] <- sum
            i <- i + 1
        arr |> Array.take i

    let checkPrime x = x < limit && Set.contains x primeSet

    let t = [for i in 0 .. primes.Length - 1 do
                let ps = primes |> Array.skip i |> partialSums
                let index = ps |> Array.findIndexBack checkPrime
                yield ps.[index], index]
    t |> List.maxBy (fun (x, y) -> y) |> fst

