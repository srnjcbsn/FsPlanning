namespace FsPlanning
module Searching =
    
    type Action<'TState> = 'TState -> 'TState * int

    type Problem<'TState> =
        { InitialState : 'TState
        ; GoalTest : 'TState -> bool
        ; Actions : 'TState -> Action<'TState> list
        }

    type SearchNode<'TState> =
        { State    : 'TState
        ; Parent   : SearchNode<'TState> option
        ; Action   : Action<'TState>
        ; PathCost : int
        }

    
    let initialNode problem =
        { State    = problem.InitialState
        ; Parent   = None
        ; Action   = (fun a -> a, 0)
        ; PathCost = 0
        }
    
    let childNode (node : SearchNode<'a>) action = 
        let (childState, stepCost) = action node.State
        { State  = childState
        ; Parent = Some node
        ; Action = action
        ; PathCost = node.PathCost + stepCost
        }

    let childNodes node problem = List.map (childNode node) <| problem.Actions node.State
