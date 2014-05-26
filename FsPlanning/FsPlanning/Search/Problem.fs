namespace FsPlanning.Search
module Problem =

    type Problem<'s, 'a, 'p> when 'a : equality and 'p : comparison =
        { InitialState : 's
        ; GoalTest     : 's -> bool
        ; Actions      : 's -> 'a list
        ; Result       : 's -> 'a -> 's
        ; StepCost     : 's -> 'a -> int
        ; Heuristic    : 's -> int -> 'p
        }

    type RegressionProblem<'s, 'a> when 'a : equality =
        { FinalState        : 's
        ; PrecedingActions  : 's -> 'a list
        ; Motives           : 's -> 'a -> 's list
        }

    [<CustomEquality; CustomComparison>]
    type SearchNode<'s, 'a> when 'a : equality and 's : equality and 's : comparison =
        { State    : 's
        ; Parent   : SearchNode<'s, 'a> option
        ; Action   : 'a option
        ; PathCost : int
        }
        override x.Equals yobj = 
            match yobj with
            | :? SearchNode<'s, 'a> as y -> (x.State = y.State)
            | _ -> false
        
        override x.GetHashCode() = hash x.State
        interface System.IComparable with
            member x.CompareTo yobj = 
                match yobj with
                | :? SearchNode<'s, 'a> as y -> 
                    compare x.State y.State
                | _ -> 0

    type Solution<'a> =
        { Path : 'a list
        ; Cost : int
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

    let solutionCost (problem : Problem<_,_,_> ) list =
        fst <| List.fold (fun (cost,state) action -> (cost + problem.StepCost state action,problem.Result state action)) (0,problem.InitialState) list 

    let rec unRavelPath node = 
        match node.Parent with
        | Some node' -> (unRavelPath node') @ [node]
        | None -> []

    let solve (solver : Problem<'s,'a,'p> -> SearchNode<'s,'a> option) problem =
        match solver problem with
        | Some solution -> Some <| { Path = unRavelPath solution; Cost = solution.PathCost }
        | None -> None

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
        let statesWithMotives = List.collect (fun action -> List.map (fun s -> (action,s) ) <| regproblem.Motives node.State action) actions
        List.map (fun (motive,state) -> childRegessionNode problem node motive state) statesWithMotives
