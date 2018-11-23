module Solutions

open Helpers

let problem1 limit =
    [1..limit - 1] |> List.filter(fun x -> x % 3 = 0 || x % 5 = 0) |> List.sum

let problem2 limit =
    fibSeq |> Seq.takeWhile(fun x -> x < limit) |> Seq.filter(fun x -> x % 2 = 0) |> Seq.sum

let problem3 number =
    primeFactors number |> set |> Seq.max

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