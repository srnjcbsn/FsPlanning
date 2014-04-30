namespace FsPlanning.Search
module Problem =

    type Problem<'s, 'a> when 'a : equality =
        { InitialState : 's
        ; GoalTest     : 's -> bool
        ; Actions      : 's -> 'a list
        ; Result       : 's -> 'a -> 's
        ; StepCost     : 's -> 'a -> int
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

