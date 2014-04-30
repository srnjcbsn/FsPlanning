namespace FsPlanning.Search
module Bidirectional =
    
    open Problem
    open FsPlanning

    type RegressionProblem<'s, 'a> when 'a : equality =
        { FinalState        : 's
        ; PrecedingActions  : 's -> 'a list
        ; Motive            : 's -> 'a -> 's list
        }


    let finalNode problem =
        { State    = problem.FinalState
        ; Parent   = None
        ; Action   = None
        ; PathCost = 0
        }

    let childRegessionNode problem (node : SearchNode<_,_>) action state = 
        { State  = state
        ; Parent = Some node
        ; Action = Some action
        ; PathCost = node.PathCost + problem.StepCost state action
        }

    let childRegessionNodes problem ( regproblem : RegressionProblem<'s, 'a>) ( node  : SearchNode<'s,'a> ) = 
        let actions = regproblem.PrecedingActions node.State
        let statesWithMotives = List.collect (fun action -> List.map (fun s -> (action,s) ) <| regproblem.Motive node.State action) actions
        List.map (fun (motive,state) -> childRegessionNode problem node motive state) statesWithMotives
//        List.map (fun state -> 
//            
//        )
//    List.map (childBiNodes problem biproblem node) <| problem.Actions node.State
    
    let updateFrontier frontier node =
        match PriorityQueue.priorityOf node frontier with
        | Some cost when cost > node.PathCost ->
            PriorityQueue.add node.PathCost node <| frontier
        | Some _ ->
            frontier
        | None ->
            PriorityQueue.add node.PathCost node <| frontier
    
    let searchNeighbours childfunc frontier explored =
        let ((_,node),frontier') = PriorityQueue.pop frontier
        let children = childfunc node
        let frontier'' = List.fold (updateFrontier) frontier' <| children
        (children,frontier'', Set.add node.State explored)

    let solve ( problem : Problem<'s, 'a>) ( regproblem : RegressionProblem<'s, 'a>)  = 
        let initNode = initialNode problem
        let finalNode = finalNode regproblem


        let rec bidirectionalSeach  normFrontier normExplored regFrontier regExplored =
            let (normChildren,normFrontier',normExplored') = searchNeighbours (childNodes problem) normFrontier normExplored
            let (regChildren,regFrontier',regExplored') = searchNeighbours (childRegessionNodes problem regproblem) regFrontier regExplored


            //let childs = childNodes problem normNode
            //let regChilds = childRegessionNodes problem regproblem regNode
            () 
        
        


        ()

