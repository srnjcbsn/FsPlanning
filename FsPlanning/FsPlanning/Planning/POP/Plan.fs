namespace FsPlanning.Planning.POP

module Plan =
    open FsPlanning.Planning.POP.Action
    open FsPlanning.Planning.POP.Types
    
    //type POPPlan = Action * 

    let find_flaw (requiredAct : ActionBody<string>) =
        let (_,_,subgoals,_) = requiredAct
        List.filter (fun sb ->  match sb with 
                                | Pos (Predicate (_,_)) -> true
                                | _ -> false) subgoals
    
    let find_flaws (requiredActs : ActionBody<string> list) =
        List.collect (fun ra -> find_flaw ra)  requiredActs

    let term_resolver (possibility : Term) (flaw : Term) =
        match flaw with
        | Pos (Predicate (fname, fvals)) ->
            match possibility with
            | Pos (Predicate (name,vals)) when name = fname ->  
                ()
            | _ -> ()
        | _ -> ()

    let attempt_resolve (flaw : Term) (action: ActionBody<string>)  : Option<ActionBody<string>*Map<Variable,int>> =
       None  

    let resolve_flaw (flaw : Term) (actions: ActionBody<string> list)  = 
        let resolvers = List.choose (attempt_resolve flaw) actions
        ()

    let solve actions initAct goalAct = 
        
        ()

    
    

    //let internal_solve flaws  
