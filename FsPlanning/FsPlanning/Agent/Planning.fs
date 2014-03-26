namespace FsPlanning.Agent
module Planning = 
    
    open FsPlanning.Searching

    type IntentionFilter =
        | Conflictive
        | Harmonic

    type DesireTree<'state,'goal> =
        | Conditional of ('state -> bool) * DesireTree<'state,'goal>
        | Desire of ('state -> 'goal option)
        | ManyDesires of DesireTree<'state,'goal> list
    
    type Planner<'TState, 'TAction,'TGoal> =
        abstract member FormulatePlan : ('TState*'TGoal) -> Solution<'TAction>