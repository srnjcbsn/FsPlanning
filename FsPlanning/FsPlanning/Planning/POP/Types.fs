namespace FsPlanning.Planning.POP
module Types = 
    
    type Variable = string
    type Constant = string

    type Value = 
        | Constant of Constant
        | Variable of Variable
    
    type  Predicate = 
        | Predicate of string * Value list
    
    let Predicate name args = Predicate(name, args)

    type Term = 
        | Pos of Predicate
        | Neg of Predicate
        | Equal of Value*Value
        | NotEqual of Value*Value
    
