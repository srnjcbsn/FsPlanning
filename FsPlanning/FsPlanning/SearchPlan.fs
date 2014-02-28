namespace FsPlanning
module SearchPlan =
//    open PriorityQueue
//
//    type PQ<'a,'b> when 'a : comparison and 'b : comparison = PriorityQueue<'a,'b>

//    type Action<'TState> = 'TState -> 'TState * int
//        interface IComparable<Action<'TState>> with
//

    type Action<'s> when 's : comparison =  
        { PreCondition  : Set<'s>
        ; EffectRemoved : Set<'s>
        ; EffectAdded   : Set<'s> 
        }

    type ProblemPlan<'s> when  's : comparison =
        { InitialState  : Set<'s>
        ; Goal          : Set<'s>
        ; Actions       : Set<'s> -> Action<'s> list
        }

    type SearchNode<'s> when  's : comparison  =
        { State    : Set<'s>
        ; Parent   : SearchNode<'s> option
        ; Action   : Action<'s> option
        ; PathCost : int
        }
    
    let initialNode problem =
        { State    = problem.InitialState
        ; Parent   = None
        ; Action   = None
        ; PathCost = 0
        }
    
    let childNode problem (node : SearchNode<'s>) (action : Action<'s>) = 
        { State  = Set.union (Set.difference node.State action.EffectRemoved) action.EffectAdded
        ; Parent = Some node
        ; Action = Some action
        ; PathCost = node.PathCost + 1
        }

    let childNodes (problem : ProblemPlan<'s>) node = List.map (childNode problem node) <| (List.filter (fun a -> Set.isSubset a.PreCondition node.State) (problem.Actions node.State))

    type Frontier<'s> when 's : comparison = SearchNode<'s> list

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

    
