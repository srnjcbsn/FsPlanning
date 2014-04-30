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
    
    type Planner<'TState, 'TAction, 'TGoal,'TSolution> =
        abstract member FormulatePlan : ('TState * 'TGoal) -> 'TSolution option
        abstract member PlanWorking : ('TState * 'TGoal * 'TSolution) -> bool
        abstract member RepairPlan : ('TState * 'TGoal * 'TSolution) -> 'TSolution option
        abstract member SolutionFinished : ('TState * 'TGoal * 'TSolution) -> bool
        abstract member NextAction : ('TState * 'TGoal * 'TSolution) -> 'TAction * 'TSolution