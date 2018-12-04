module Helpers

let fibSeq = Seq.unfold (fun (a,b) -> Some( a+b, (b, a+b) ) ) (0,1)

let naturalNumbersFrom (n:int64) = Seq.unfold (fun (x:int64) -> Some(x, x+1L)) n

let triangularNumbers = Seq.unfold(fun (acc, num) -> Some(num + acc, (num + acc, num + 1))) (0, 1)

let square n = n * n

let (|Even|Odd|) x = if x % 2 = 0 then Even else Odd

let digits n =
    n.ToString().ToCharArray() |> Array.map(fun c -> c.ToString()) |> Array.map int

let product x =
    x |> Seq.reduce(fun x y -> x * y)

let bigProduct (x:seq<bigint>) =
    x |> Seq.reduce(fun x y -> x * y)

let factorial n =
    [1I .. n] |> bigProduct

let isPrime (n:int64) =
    match n with
    | x when x = 2L -> true
    | x when x % 2L = 0L || x < 2L -> false
    | x -> let maxFactor = int64 ((floor (sqrt (float x))) + 1.0)
           let factors = [3L..2L..maxFactor] |> List.filter(fun t -> x % t = 0L)
           List.isEmpty factors

let factorsLong (n:int64) =
    let limit = int64 (ceil (sqrt (float n)))
    let small = [2L..limit] |> List.filter(fun x -> n % x = 0L)
    let large = small |> List.map(fun x -> n / x)
    small @ large

let factors n =
    let limit = int (ceil (sqrt (float n)))
    let small = [2 .. limit] |> List.filter(fun x -> n % x = 0)
    let large = small |> List.map(fun x -> n / x)
    1 :: n :: small @ large

let primeFactorsLong (n:int64) =
    factorsLong n |> List.filter isPrime

let primeFactors n =
    factors n |> List.map int64 |> List.filter isPrime

let isPalindrome num =
    let s = num.ToString().ToCharArray()
    let t = s |> Array.rev
    s = t

let rec gcd n m =
    match m with
        | 0L -> n
        | _ -> gcd m (n % m)

let lcm n m =
    n * m / gcd n m

let rec sift (a:list<int>) =
    match a with
    | [] -> []
    | h::xs -> h :: sift (xs |> List.filter (fun y -> y % h <> 0))

let primesUpTo n =
    2 :: sift [3..2..n]

let pythagoreanTriplets n =
    seq {
        for a in [1 .. n/3] do
            for b in [a .. n/2] do
                yield (a, b, n - a - b)
    }

let numOfDistinctFactors x =
    factors x |> List.distinct |> List.length

let collatzSequence = Seq.unfold (fun (s:int64) -> if s = 0L then None else if s = 1L then Some(s, 0L) else Some(s, if s % 2L = 0L then (s / 2L) else (3L * s + 1L)))

// only works with rectangle grid
let rightDiagonals (x:'a[][]) =
    let maxRowIndex = x.[0].Length - 1
    let maxColumnIndex = (x |> Array.map (fun z -> z.[0])).Length - 1

    let rowDiagonalsSeq =
        seq {
            for i in [0 .. maxRowIndex] do
                yield seq {
                    let mutable k = 0;
                    while i + k <= maxRowIndex && k <= maxColumnIndex do
                        yield x.[k].[i + k]
                        k <- k + 1
                }
        }

    let columnDiagonalsSeq =
        seq {
            // we start from 1, because longest diagonal was already done with rows
            for i in [1 .. maxColumnIndex] do
                yield seq {
                    let mutable k = 0;
                    while i + k <= maxColumnIndex && k <= maxRowIndex do
                        yield x.[i + k].[k]
                        k <- k + 1
                }
        }

    Seq.append columnDiagonalsSeq rowDiagonalsSeq

let leftDiagonals (x:'a[][]) =
    let maxRowIndex = x.[0].Length - 1
    let maxColumnIndex = (x |> Array.map (fun z -> z.[0])).Length - 1

    let rowDiagonalsSeq =
        seq {
            for i in [maxRowIndex .. -1 .. 0] do
                yield seq {
                    let mutable k = 0;
                    while i - k >= 0 && k <= maxColumnIndex do
                        yield x.[k].[i - k]
                        k <- k + 1
                }
        }

    let columnDiagonalsSeq =
        seq {
            for i in [1 .. maxColumnIndex] do
                yield seq {
                    let mutable k = 0;
                    while i + k <= maxRowIndex && k <= maxColumnIndex do
                        yield x.[i + k].[maxColumnIndex - k]
                        k <- k + 1
                }
        }

    Seq.append rowDiagonalsSeq columnDiagonalsSeq

let countLettersInNumber (map:Map<int, int>) n =
    let mutable count = 0;

    if n = 1000 then
        count <- count + map.[1] + map.[1000]
    else
        let hundreds = n / 100
        let rest = n % 100
        if (hundreds > 0) then
            count <- count + map.[hundreds] + map.[100]

        let tens = rest / 10
        let rest2 = rest % 10
        if (tens > 1) then
            if (hundreds > 0) then
                count <- count + 3 // and
            count <- count + map.[rest - rest2]
        if (tens = 1) then
            if (hundreds > 0) then
                count <- count + 3 // and
            count <- count + map.[rest]
        else if (rest2 <> 0) then
            if ((hundreds > 0) && (tens = 0)) then
                count <- count + 3 // and
            count <- count + map.[rest2]

    count
