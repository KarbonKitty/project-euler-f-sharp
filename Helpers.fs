module Helpers

let fibSeq = Seq.unfold (fun (a,b) -> Some( a+b, (b, a+b) ) ) (0,1)

let naturalNumbersFrom (n:int64) = Seq.unfold (fun (x:int64) -> Some(x, x+1L)) n

let square n = n * n

let isPrime (n:int64) =
    match n with
    | x when x = 2L -> true
    | x when x % 2L = 0L || x < 2L -> false
    | x -> let maxFactor = int64 ((floor (sqrt (float x))) + 1.0)
           let factors = [3L..2L..maxFactor] |> List.filter(fun t -> x % t = 0L)
           List.isEmpty factors

let factors (n:int64) =
    let limit = int64 (ceil (sqrt (float n)))
    let small = [2L..limit] |> List.filter(fun x -> n % x = 0L)
    let large = small |> List.map(fun x -> n / x)
    small @ large

let primeFactors (n:int64) =
    factors n |> List.filter isPrime

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