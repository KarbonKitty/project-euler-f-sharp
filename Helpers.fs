module Helpers

let fibSeq = Seq.unfold (fun (a,b) -> Some( b, (b, a+b) ) ) (0,1)

let largeFib = Seq.unfold (fun (a, b) -> Some( b, (b, a + b))) (0I, 1I)

let naturalNumbersFrom (n:int64) = Seq.unfold (fun (x:int64) -> Some(x, x+1L)) n

let triangularNumbers = Seq.unfold(fun (acc, num) -> Some(num + acc, (num + acc, num + 1))) (0, 1)

let square n = n * n

let (|Even|Odd|) x = if x % 2 = 0 then Even else Odd

let letterValues = [ 'A', 1; 'B', 2; 'C', 3; 'D', 4; 'E', 5; 'F', 6; 'G', 7; 'H', 8; 'I', 9; 'J', 10; 'K', 11; 'L', 12; 'M', 13; 'N', 14; 'O', 15; 'P', 16; 'Q', 17; 'R', 18; 'S', 19; 'T', 20; 'U', 21; 'V', 22; 'W', 23; 'X', 24; 'Y', 25; 'Z', 26 ] |> Map.ofList

let digits n =
    n.ToString().ToCharArray() |> Array.map(fun c -> c.ToString()) |> Array.map int

let isPandigital x =
    let digitsArr = digits x
    let allDigits = Set [1; 2; 3; 4; 5; 6; 7; 8; 9]
    if digitsArr.Length <> allDigits.Count then
        false
    else
        (digitsArr |> Set.ofArray) = allDigits

let isKPandigital k x =
    let digitsArr = digits x
    let digitsTillK = Set [ 1 .. k ]
    if digitsArr.Length <> digitsTillK.Count then
        false
    else (digitsArr |> Set.ofArray) = digitsTillK

let isShortPandigital x =
    isKPandigital (digits x |> Array.length) x

let product x =
    x |> Seq.reduce(fun x y -> x * y)

let bigProduct (x:seq<bigint>) =
    x |> Seq.reduce(fun x y -> x * y)

let smallFactorial n =
    match n with
    | 0 -> 1
    | _ -> [1 .. n] |> product

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

let properDivisors n =
    let limit = int (ceil (sqrt (float n)))
    let small = [2 .. limit] |> List.filter(fun x -> n % x = 0)
    let large = small |> List.map(fun x -> n / x)
    1 :: small @ large |> List.distinct

let factors n =
    n :: properDivisors n

let primeFactorsLong (n:int64) =
    factorsLong n |> List.filter isPrime

let primeFactors n =
    factors n |> List.map int64 |> List.filter isPrime

let isAbundant x =
    (properDivisors x |> List.sum) > x

let isPalindrome num =
    let s = num.ToString().ToCharArray()
    let t = s |> Array.rev
    s = t

let isPalindromeInBase (b:int) (num:int) =
    let s = System.Convert.ToString(num, b).ToCharArray()
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

let sieve limit =
    let mutable current = 3
    let mutable primes = [2]
    let mutable numbers = [|3 .. 2 .. limit|]
    while (current * current) < limit do
        current <- numbers |> Array.head
        primes <- current :: primes
        numbers <- numbers |> Array.filter (fun x -> x % current <> 0)
    primes @ List.ofArray numbers

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

let areAmicable x y =
    (properDivisors x |> List.sum = y) && (properDivisors y |> List.sum = x)

// terrible copy of C# variant, but no better idea for now
let dfcl n =
    let mutable rs = []
    let mutable max = 0
    let mutable chosen = 0
    for d in [2 .. n] do
        let mutable found = false
        let mutable a = 1
        while a <> 0 do 
            let mutable rest = a % d
            if ((not found) && (rs |> List.contains rest)) then
                found <- true
                rs <- []
            else if (found && (rs |> List.contains rest)) then
                if (rs.Length > max) then
                    max <- rs.Length
                    chosen <- d
                rest <- 0
            else
                rs <- rest :: rs
            a <- rest * 10
    chosen

let fifthPowersOfDigits = [0 .. 9] |> List.map (fun x -> pown x 5)

let fifthPowersOfDigitsSum n =
    let digits = digits n
    digits |> Array.map (fun x -> fifthPowersOfDigits.[x]) |> Array.sum

let exceptLast xs =
    let last = xs |> List.last
    xs |> List.except last

let coins = [1; 2; 5; 10; 20; 50; 100; 200]

// based on http://www.algorithmist.com/index.php/Coin_Change
// TODO: memoization might be nice, but it's still less than 200 ms
let rec coinCount total index =
    match total with
    | 0 -> 1
    | _ when total < 0 -> 0
    | _ when index <= 0 && total >= 1 -> 0
    | _ -> (coinCount total (index - 1)) + (coinCount (total - coins.[index - 1]) index)

let digitFactorials = [0 .. 9] |> List.map smallFactorial

let digitFactorialsSum n =
    let digits = digits n
    digits |> Array.map (fun x -> digitFactorials.[x]) |> Array.sum

let rotate x =
    let lastDigit = x % 10
    let numberOfDigits = digits x |> Array.length
    x / 10 + (lastDigit * pown 10 (numberOfDigits - 1))
