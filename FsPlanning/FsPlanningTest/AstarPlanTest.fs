namespace FsPlanningTest
open NUnit.Framework
open FsPlanning.AStarPlan
open FsPlanning.SearchPlan
    
    type fact<'f> = string*('f list) 
    type ball<'b> = 'b 
    
    [<TestFixture>]
    type AStarPlanTest() = 
            
            

            [<Test>]
            member this.Solve_BiB_FindSolution() = 
                let acts b = list.Empty
                let problem = { InitialState = Set.ofList [ball "a"];  Goal = Set.empty; Actions = acts } 
                ()
                