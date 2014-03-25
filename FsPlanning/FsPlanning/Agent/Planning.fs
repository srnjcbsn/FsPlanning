namespace FsPlanning.Agent
module Planning = 
    
    type IntentionFilter =
        | Conflictive
        | Harmonic

    type DesireTree<'state,'goal> =
        | Conditional of ('state -> bool) * DesireTree<'state,'goal>
        | Desire of ('state -> 'goal option)
        | ManyDesires of DesireTree<'state,'goal> list
    