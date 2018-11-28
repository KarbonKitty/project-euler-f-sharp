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
    let x1 = a |> Array.map (fun x -> x |> Seq.windowed y |> Seq.map product |> Seq.max) |> Seq.max
    x1

let problem12 numOfDivisors =
    triangularNumbers |> Seq.map (fun x -> (x, numOfDistinctFactors x)) |> Seq.find (fun (_, n) -> n > numOfDivisors)

let problem13 (xs:string[]) =
    xs |> Array.map (fun x -> BigInteger.Parse(x)) |> Array.sum