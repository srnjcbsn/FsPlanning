namespace FsPlanning.Searching
module AStar =
    open FsPlanning.Searching

    let updateFrontier explored frontier node = 
            let inExplored = Set.contains node.State explored
            let priority = priorityOf node.State frontier
            match (inExplored, priority) with
            | (false, None)                      -> insert node frontier
            | (_, Some p) when p > node.PathCost -> insert node frontier
            | (_, _)                             -> frontier 

    let rec aStar' problem frontier explored =
        match pop frontier with
        | None -> None
        | Some (bestNode, frontier') -> 
            let explored = Set.add bestNode.State explored
            let frontier'' = List.fold (updateFrontier explored) frontier' <| childNodes problem bestNode

            match problem.GoalTest bestNode.State with
            | true  -> Some bestNode
            | false -> aStar' problem frontier'' explored

    let Solve problem =
        let node = initialNode problem
        aStar' problem [node] Set.empty


