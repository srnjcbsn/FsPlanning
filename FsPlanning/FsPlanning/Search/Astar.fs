namespace FsPlanning.Search
module Astar =
    open System
    open FsPlanning
    open Problem
    open System
    
    let aStar problem breakTest =

        let updateFrontier explored frontier node = 
            //lock flock (fun () -> printfn "Frontier size: %A" (List.length frontier))
            let inExplored = Set.contains node.State explored
            let priority = PriorityQueue.priorityOf node frontier
            let newPriority = problem.Heuristic node.State node.PathCost
            match (inExplored, priority) with
            | (false, None)                      -> PriorityQueue.add newPriority node frontier
            | (_, Some p) when p > newPriority   -> 
                PriorityQueue.remove p node frontier
                |> PriorityQueue.add newPriority node
            | (_, _)                             -> frontier 

        let rec aStar' frontier explored =
            match PriorityQueue.tryPop frontier with
            | None -> None
            | Some ((_, bestNode), frontier') -> 

                let explored = Set.add bestNode.State explored
                let children = childNodes problem bestNode
                let frontier'' = List.fold (updateFrontier explored) frontier' <| children

                match (problem.GoalTest bestNode.State, breakTest()) with
                | (false, false) -> aStar' frontier'' explored
                | _ -> Some bestNode
        
        let node = initialNode problem
        let g = problem.Heuristic node.State node.PathCost 
        aStar' (PriorityQueue [(g, node)]) Set.empty

    let solveSearchNodePath (solver : Problem<'s,'a,'p> -> (unit -> bool) -> SearchNode<'s,'a> option) problem breakTest =
        match solver problem breakTest with
        | Some solution -> Some <| {Path = unRavelPath solution; Cost = solution.PathCost}
        | None -> None

    let aStarAllPaths problem =
        let updateFrontier explored frontier node = 
            let inExplored = Set.contains node.State explored
            let priority = PriorityQueue.priorityOf node frontier
            let newPriority = problem.Heuristic node.State node.PathCost
            match (inExplored, priority) with
            | (false, None)                      -> PriorityQueue.add newPriority node frontier
            | (_, Some p) when p > newPriority   -> PriorityQueue.add newPriority node frontier
            | (_, _)                             -> frontier 

        let rec aStar' frontier explored paths=
            match PriorityQueue.tryPop frontier with
            | None -> paths
            | Some ((_,bestNode), frontier') -> 
                let explored = Set.add bestNode.State explored
                let children = childNodes problem bestNode
                let frontier'' = List.fold (updateFrontier explored) frontier' <| children
                aStar' frontier'' explored (bestNode::paths)
        
        let node = initialNode problem
        let g = problem.Heuristic node.State node.PathCost 
        aStar' (PriorityQueue [(g, node)]) Set.empty List.empty

    let allStatesWithCost (nodes : SearchNode<_,_> list) =
        List.map (fun n -> (n.PathCost,n.State) ) nodes

    let solve (solver : Problem<'s,'a,'p> -> (unit -> bool) -> SearchNode<'s,'a> option) problem breakTest =
        let solved = solveSearchNodePath solver problem breakTest
        match solved with
        | Some {Path = path; Cost = cost} -> Some {Path = List.map (fun node -> node.Action.Value) path; Cost = cost}
        | None -> None
