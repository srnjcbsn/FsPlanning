
// NOTE: If warnings appear, you may need to retarget this project to .NET 4.0. Show the Solution
// Pad, right-click on the project node, choose 'Options --> Build --> General' and change the target
// framework to .NET 4.0 or .NET 4.5.

module BallsInBaskets.Main

open System
open BIB
open FsPlanning

let someFunction x y = x + y

[<EntryPoint>]
let main args = 
    let prob = problem 3 (Map [(1,1);(2,2);(3,3)]) ((=) (Map [(1,2);(2,3);(3,1)]))
    printfn "%A" <| AStar.Solve prob
    0

