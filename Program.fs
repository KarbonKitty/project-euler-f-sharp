module Program

[<EntryPoint>]
let main argv =
    let stopWatch = System.Diagnostics.Stopwatch.StartNew()

    // printfn "%i" (Solutions.problem1 1000)
    // printfn "%i" (Solutions.problem2 4000000)
    // printfn "%i" (Solutions.problem3 600851475143L)
    // printfn "%i" (Solutions.problem4 999)
    // printfn "%i" (Solutions.problem5 20L)
    // printfn "%i" (Solutions.problem6 100)
    // printfn "%i" (Solutions.problem7 10001)
    // printfn "%O" (Solutions.problem8 (File.ReadAllText @"data\8.txt") 13)
    // printfn "%i" (Solutions.problem9 1000)
    // printfn "%i" (Solutions.problem10 2000000L)
    // printfn "%A" (Solutions.problem11 (File.ReadAllLines @"data\11.txt") 4)
    // printfn "%A" (Solutions.problem12 500)
    // printfn "%A" (Solutions.problem13 (File.ReadAllLines @"data\13.txt"))
    // printfn "%A" (Solutions.problem14 500000L 1000000L)
    printfn "%A" (Solutions.problem15 20I)

    stopWatch.Stop()

    printfn "Time elapsed: %i ms" stopWatch.ElapsedMilliseconds
    0 // return an integer exit code
