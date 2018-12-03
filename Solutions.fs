module Solutions

open Helpers
open System.Numerics

let problem1 limit =
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
