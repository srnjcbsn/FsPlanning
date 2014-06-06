namespace FsPlanning.Agent
module Planning = 
    
    open FsPlanning.Search.Problem
    open FsPlanning.Search.Astar

    type IntentionFilter =
        | Conflictive
        | Harmonic

    type DesireTree<'state,'goal> =
        | Conditional of ('state -> bool) * DesireTree<'state,'goal>
        | Desire of ('state -> 'goal option)
        | ManyDesires of DesireTree<'state,'goal> list
    
    // Put priority to 0, and it will give all desires(in a list) for which their condition is true
    let rec travelDesires prio state (dTree:DesireTree<'TState,'TIntention>) = 
        match dTree with
        | Conditional (c,t) ->
            let lastprio,goals = travelDesires prio state t
            if c(state) then
                (lastprio,goals)
            else
                (lastprio,[])
        | Desire cG ->
            let lp = prio + 1
            (lp,[(lp,cG)])
                   
        | ManyDesires tree -> 
            List.fold   (fun (p,ds) t -> 
                                let (newp,newDs) = travelDesires p state t
                                (newp,ds@newDs)
                        ) (prio,[]) tree 

    type Planner<'TState, 'TAction, 'TGoal,'TSolution> =
        abstract member FormulatePlan : 'TState * 'TGoal -> 'TSolution option
        //abstract member PlanWorking : 'TState * 'TGoal * 'TSolution -> bool
        abstract member RepairPlan : 'TState * 'TGoal * 'TSolution -> 'TSolution option
        abstract member SolutionFinished : 'TState * 'TGoal * 'TSolution -> bool
        abstract member NextAction : 'TState * 'TGoal * 'TSolution -> ('TAction * 'TSolution) option
        abstract member UpdateStateBeforePlanning : 'TState * 'TGoal -> 'TState
        abstract member UpdateStateOnSolutionFinished : 'TState * 'TGoal * 'TSolution -> 'TState