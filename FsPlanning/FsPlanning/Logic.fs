namespace FsPlanning
module Logic = 
    
    type Literal<'Atom> =
        | Pos of 'Atom
        | Neg of 'Atom
    
    type LogicTerm<'Atom> = 
        | Literal of Literal<'Atom>
        | And of LogicTerm<'Atom> * LogicTerm<'Atom>
        | Or of LogicTerm<'Atom> * LogicTerm<'Atom> 
