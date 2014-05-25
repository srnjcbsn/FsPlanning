namespace FsPlanning.Search
module Astar =
    open System
    open FsPlanning
//    open PriorityQueue
//
//    type PQ<'a,'b> when 'a : comparison and 'b : comparison = PriorityQueue<'a,'b>

//    type Action<'TState> = 'TState -> 'TState * int
//        interface IComparable<Action<'TState>> with
//
    open Problem

    //type Frontier<'s,'a> when 'a : equality and 's : comparison = SearchNode<'s,'a> list
    

//    let nodeWithState state frontier = List.tryFind (fun node -> node.State = state) frontier
//
//    let priorityOf state frontier = 
//        match (nodeWithState state frontier) with
//        | Some node -> Some node.PathCost
//        | None -> None
//
//    let insert node frontier = 
//        let prunedFrontier = List.filter (fun lnode -> lnode.State <> node.State) frontier
//        List.sortBy (fun node -> node.PathCost) (node :: prunedFrontier)
//    
//    let pop frontier = 
//        match frontier with
//        | h :: t -> Some (h, t)
//        | _ -> None

    //let flock = new Object();
    
    let aStar problem =

        let updateFrontier explored frontier node = 
            //lock flock (fun () -> printfn "Frontier size: %A" (List.length frontier))
            let inExplored = Set.contains node.State explored
            let priority = PriorityQueue.priorityOf node frontier
            match (inExplored, priority) with
            | (false, None)                      -> PriorityQueue.add node.PathCost node frontier
            | (_, Some p) when p > node.PathCost -> PriorityQueue.add node.PathCost node frontier
            | (_, _)                             -> frontier 

        let rec aStar' frontier explored =
            match PriorityQueue.tryPop frontier with
            | None -> None
            | Some ((_,bestNode), frontier') -> 
                
                let explored = Set.add bestNode.State explored
                let children = childNodes problem bestNode
                let frontier'' = List.fold (updateFrontier explored) frontier' <| children
                
                match problem.GoalTest bestNode.State with
                | true  -> Some bestNode
                | false -> aStar' frontier'' explored
        
        let node = initialNode problem
        aStar' (PriorityQueue [(node.PathCost,node)]) Set.empty

   

    let solve (solver : Problem<'s,'a> -> SearchNode<'s,'a> option) problem =
        match solver problem with
        | Some solution -> Some <| { Path = unRavelPath solution; Cost = solution.PathCost }
        | None -> None

