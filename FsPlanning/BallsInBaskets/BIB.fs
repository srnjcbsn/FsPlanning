namespace BallsInBaskets
module BIB =
    
    open FsPlanning.Searching

    type Ball = int
    type Basket = int
    type BibState = Map<Ball, Basket>

    type BibAction = Move of Ball * Basket

    let actionsForBall numberOfBaskets ball basket = 
        List.map (fun i -> Move (ball, i)) [for i in 1 .. numberOfBaskets do if i <> basket then yield i]

    let actions numberOfBaskets state = 
        List.concat (List.map (fun (ball, basket) -> actionsForBall numberOfBaskets ball basket) <| Map.toList state)

    let result state (Move (ball, basket)) = Map.add ball basket state

    let stepCost state action = 1

    let problem numberOfBaskets initialState goalTest = 
        { InitialState = initialState
        ; GoalTest = goalTest
        ; Actions = actions numberOfBaskets
        ; Result = result
        ; StepCost = stepCost
        }

    