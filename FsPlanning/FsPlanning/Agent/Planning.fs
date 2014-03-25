namespace FsPlanning.Agent
module Planning = 
    
    type DesireTree<'state,'goal> =
        | Conditional of ('state -> bool) * DesireTree<'state,'goal>
        | Desire of ('state -> 'goal option)
        | ManyDesires of DesireTree<'state,'goal> list
    