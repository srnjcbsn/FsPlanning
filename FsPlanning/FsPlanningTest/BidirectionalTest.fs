namespace FsPlanningTest
open System
open NUnit.Framework
open FsPlanning.Search.Problem
open FsPlanning.Search

    type action =
        | Move of int
        override x.ToString() = 
            let (Move p) = x
            p.ToString()

    type state = 
        | Pos of int
    


    [<TestFixture>]
    type BidirectionalTest() = 
        

        let makeProblem start finish graph = 
            { InitialState  = Pos start
            ; GoalTest      = fun s -> s = Pos finish
            ; Actions       = fun (Pos node) -> List.map (fun n -> Move n) <| SimpleGraph.neighbours node graph
            ; Result        = fun s (Move node) -> Pos node 
            ; StepCost      = fun s a -> 1
            }
        
        let makeRegProblem finish graph =
            { FinalState        = Pos finish
            ; PrecedingActions  = fun (Pos node) -> [Move node]
            ; Motives           = fun (Pos node) a -> List.map (fun n -> Pos n) <| SimpleGraph.neighbours node graph
            }

        [<Test>]
        member this.Solve_SolvableGraph_SolvedProblem () =
            let start = 1
            let finish = 12
            let edges = 
                [   (1,2); (1,5); (5,6); (6,7)
                ;   (2,3)
                ;   (3,4)
                ;   (4,9)
                ;   (9,10)
                ;   (10,11)
                ;   (11,12)
                ]
                    
            let graph = SimpleGraph.ofList edges
            let problem = makeProblem start finish graph
            let regProblem = makeRegProblem finish graph
            
            let solution = Bidirectional.solve problem regProblem
            Assert.AreEqual (Some <| List.map (fun i -> Move i) [2;3;4;9;10;11;12],solution)

        [<Test>]
        member this.Solve_UnSolvableGraph_NoSolutionButTerminated () =
            let start = 1
            let finish = 12
            let edges = 
                [   (1,2); (1,5); (5,6); (6,7)
                ;   (2,3)
                ;   (3,4)
                ;   (9,10)
                ;   (10,11)
                ;   (11,12)
                ]
                    
            let graph = SimpleGraph.ofList edges
            let problem = makeProblem start finish graph
            let regProblem = makeRegProblem finish graph
                
            let solution = Bidirectional.solve problem regProblem

            Assert.AreEqual (None,solution)