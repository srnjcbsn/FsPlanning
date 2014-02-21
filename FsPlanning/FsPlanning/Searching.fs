﻿namespace FsPlanning
module Searching =
//    open PriorityQueue
//
//    type PQ<'a,'b> when 'a : comparison and 'b : comparison = PriorityQueue<'a,'b>

//    type Action<'TState> = 'TState -> 'TState * int
//        interface IComparable<Action<'TState>> with
//

    type Problem<'s, 'a> when 'a : equality =
        { InitialState : 's
        ; GoalTest     : 's -> bool
        ; Actions      : 's -> 'a list
        ; Result       : 's -> 'a -> 's
        ; StepCost     : 's -> 'a -> int
        }

    type SearchNode<'s, 'a> when 'a : equality =
        { State    : 's
        ; Parent   : SearchNode<'s, 'a> option
        ; Action   : 'a option
        ; PathCost : int
        }
    
    let initialNode problem =
        { State    = problem.InitialState
        ; Parent   = None
        ; Action   = None
        ; PathCost = 0
        }
    
    let childNode problem (node : SearchNode<'s,'a>) action = 
        { State  = problem.Result node.State action
        ; Parent = Some node
        ; Action = Some action
        ; PathCost = node.PathCost + problem.StepCost node.State action
        }

    let childNodes problem node = List.map (childNode problem node) <| problem.Actions node.State

    type Frontier<'s,'a> when 'a : equality = SearchNode<'s,'a> list

    let nodeWithState state frontier = List.tryFind (fun node -> node.State = state) frontier

    let priorityOf state frontier = 
        match (nodeWithState state frontier) with
        | Some node -> Some node.PathCost
        | None -> None

    let insert node frontier = 
        let prunedFrontier = List.filter (fun lnode -> lnode.State <> node.State) frontier
        List.sortBy (fun node -> node.PathCost) (node :: prunedFrontier)
    
    let pop frontier = 
        match frontier with
        | h :: t -> Some (h, t)
        | _ -> None

    let aStar problem =

        let updateFrontier explored frontier node = 
            let inExplored = Set.contains node.State explored
            let priority = priorityOf node.State frontier
            match (inExplored, priority) with
            | (false, None)                      -> insert node frontier
            | (_, Some p) when p > node.PathCost -> insert node frontier
            | (_, _)                             -> frontier 

        let rec aStar' frontier explored =
            match pop frontier with
            | None -> None
            | Some (bestNode, frontier') -> 
                let explored = Set.add bestNode.State explored
                let frontier'' = List.fold (updateFrontier explored) frontier' <| childNodes problem bestNode

                match problem.GoalTest bestNode.State with
                | true  -> Some bestNode
                | false -> aStar' frontier'' explored
        
        let node = initialNode problem
        aStar' [node] Set.empty
