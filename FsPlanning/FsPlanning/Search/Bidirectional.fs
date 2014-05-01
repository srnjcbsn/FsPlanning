namespace FsPlanning.Search
module Bidirectional =
    
    open Problem
    open FsPlanning

  
    
    let updateFrontier frontier node =
        match PriorityQueue.priorityOf node frontier with
        | Some cost when cost > node.PathCost ->
            PriorityQueue.add node.PathCost node <| frontier
        | Some _ ->
            frontier
        | None ->
            PriorityQueue.add node.PathCost node <| frontier
    
    let searchNeighbours childfunc frontier explored =
        match PriorityQueue.tryPop frontier with
        | Some ((_,node),frontier') ->
            let children = List.filter (fun child -> not <| Set.contains child.State explored ) <| childfunc node

            let frontier'' = List.fold (updateFrontier) frontier' <| children
            Some (children,frontier'', Set.add node.State explored)
        | _ -> None
    
 

    let doesNodeReachGoal problem (node : SearchNode<_,_>) (regNode : SearchNode<_,_>) =
        let actionPath = List.rev <| unRavelPath regNode
        let lastState = List.fold (fun state action -> problem.Result state action) node.State actionPath
        problem.GoalTest lastState

    let doesFrontNodeHitBackNodes problem node (regressionNodes : List<_>) =
        let acts = (problem.Actions node.State)
        List.tryPick (fun regnode ->
                        let reachGoal = (List.exists (fun act -> match regnode.Action with
                                                    | Some regAct -> act = regAct && doesNodeReachGoal problem node regnode
                                                    | None -> false
                                    ) acts )
                        if reachGoal then
                            Some regnode
                        else
                            None

                    ) <| regressionNodes
    
    let tryFindBackNodeThatHitFrontNodes problem regNode (progressionNodes : List<_>) =
        List.tryPick (fun node ->
                        match (node.Action,regNode.Action) with
                        | (Some act,Some regAct) when act = regAct && doesNodeReachGoal problem node regNode ->
                            Some (node,regNode)
                        | _ -> None

                    ) <| progressionNodes
    
    let nodesFromPQ pq =List.map (snd) <| PriorityQueue.toList pq

    let attemptedGoalFromFront problem proChildren regNodes = 
        List.tryPick 
            (fun n -> 
                if problem.GoalTest n.State then
                    Some (n,None)
                else
                    let regNode = doesFrontNodeHitBackNodes problem n regNodes 
                    if regNode.IsSome  then
                        Some (n,regNode)
                    else
                        None
            ) proChildren

    let attemptedGoalNodeFromBack problem regChildren proNodes =
        List.tryPick (fun n -> tryFindBackNodeThatHitFrontNodes problem n proNodes) regChildren
    
    let combineNodePath proNode regNode = (unRavelPath proNode)@(List.rev <| unRavelPath regNode)

    let solve ( problem : Problem<'s, 'a>) ( regproblem : RegressionProblem<'s, 'a>)  = 
        let initNode = initialNode problem
        let finalNode = finalNode regproblem


        let rec bidirectionalSeach  proFrontier proExplored regFrontier regExplored =
            let front = searchNeighbours (childNodes problem) proFrontier proExplored
            let back = searchNeighbours (childRegessionNodes problem regproblem) regFrontier regExplored

            match (front,back) with
            |   ( Some (proChildren,proFrontier',proExplored')
                , Some (regChildren,regFrontier',regExplored') ) ->  
                let tryGoal = attemptedGoalFromFront problem proChildren (nodesFromPQ regFrontier')
                let tryGoalFromBack = attemptedGoalNodeFromBack problem regChildren (nodesFromPQ  proFrontier')

                let solFront =  match tryGoal with
                                | Some (proGoal, None) -> Some <| unRavelPath proGoal
                                | Some (proGoal, Some regNode) -> Some <| combineNodePath proGoal regNode
                                | None -> None
                let solBack =   match tryGoalFromBack with
                                | Some (proNode, regNode) -> Some <| combineNodePath proNode regNode
                                | None -> None
                match (solFront,solBack) with
                | (Some sol,None) -> Some sol
                | (None, Some sol) -> Some sol
                | (Some solFront,Some solBack) ->
                    let costFront = (solutionCost problem solFront)
                    let costBack = (solutionCost problem solBack)
                    if costFront <= costFront then
                        Some solFront
                    else
                        Some solBack
                | (None, None) -> bidirectionalSeach proFrontier' proExplored' regFrontier' regExplored'
                     
                     
            |  ( Some (proChildren,proFrontier',proExplored'), None) ->
                let tryGoal = List.tryFind (fun n -> problem.GoalTest n.State) proChildren
                match tryGoal with
                | Some goal -> Some <| unRavelPath goal
                | None -> bidirectionalSeach proFrontier' proExplored' regFrontier regExplored
            | _ -> None

        bidirectionalSeach (PriorityQueue [(initNode.PathCost,initNode)]) Set.empty (PriorityQueue [(finalNode.PathCost,finalNode)]) Set.empty

