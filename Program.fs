module Program

open System
open System.IO
open Helpers

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
    // printfn "%A" (Solutions.problem15 20I)
    // printfn "%A" (Solutions.problem16 1000)
    // printfn "%A" (Solutions.problem17 (File.ReadAllLines @"data\17.txt") 1000)
    // printfn "%A" (Solutions.problem18 (File.ReadAllLines @"data\18.txt"))
    // printfn "%A" (Solutions.problem19 (DateTime.Parse "01.01.1901") (DateTime.Parse "31.12.2000"))
    // printfn "%A" (Solutions.problem20 100I)
    // printfn "%A" (Solutions.problem21 10000)
    // printfn "%A" (Solutions.problem22 (File.ReadAllLines @"data\22.txt"))
    // printfn "%A" (Solutions.problem23 28123)
    // printfn "%A" (Solutions.problem24 [0; 1; 2; 3; 4; 5; 6; 7; 8; 9] 1000000)
    // printfn "%A" (Solutions.problem25 1000)
    // printfn "%A" (Solutions.problem26 1000)
    // printfn "%A" (Solutions.problem27 1000 1000)
    // printfn "%A" (Solutions.problem28 1001)
    // printfn "%A" (Solutions.problem29 100I 100)
    // printfn "%A" (Solutions.problem30 ())
    // printfn "%A" (Solutions.problem31 200)
    // printfn "%A" (Solutions.problem32 ())
    // printfn "%A" (Solutions.problem33 ())
    // printfn "%A" (Solutions.problem34 ())
    // printfn "%A" (Solutions.problem35 1000000)
    // printfn "%A" (Solutions.problem36 1000000)
    // printfn "%A" (Solutions.problem37 ())
    // printfn "%A" (Solutions.problem38 ())
    // printfn "%A" (Solutions.problem39 1000)
    // printfn "%A" (Solutions.problem40 ())
    printfn "%A" (Solutions.problem41 ())

    stopWatch.Stop()

    printfn "Time elapsed: %i ms" stopWatch.ElapsedMilliseconds
    0 // return an integer exit code
