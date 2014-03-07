namespace FsPlanning.Planning.POP
module Action = 
    
    open FsPlanning.Planning.POP.Types
    

    type Parameter<'identifier> =
        | Parameter of 'identifier

    type ActionBody<'identifier> = string * Parameter<'identifier> list * Term list * Term list

    let AllPosPredicates = List.map (fun t -> Pos t)

    let Action name parameters precond effects = (name,parameters,precond,effects):ActionBody<_>


    module Parameter = 
        
        let VariableOf arg = 
            match arg with
            | Parameter a -> Variable a
        