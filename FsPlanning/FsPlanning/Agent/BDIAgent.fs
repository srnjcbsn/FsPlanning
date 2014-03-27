namespace FsPlanning.Agent

    open System
    open System.Threading;
    open Planning

    [<AbstractClass>]
    type public BDIAgent<'TPercept,'TState,'TAction,'TIntention,'TSolution when 'TState : comparison>(initstate,desires,planner) = class 
        
        let desires:DesireTree<'TState,'TIntention> = desires
        let planner:Planner<'TState, 'TAction,'TIntention,'TSolution> = planner
        let mutable intentionIdCounter = 0L
        let mutable intentions = Map.empty
        let mutable solutions = Map.empty
        let mutable state = initstate
        let mutable actuators = []
        let mutable sensors = []

        let mutable runningSolution = None
        let mutable solutionsOnHold = []

        //locks
        let stateLock = new Object()
        let solutionsLock = new Object()
        let runningSolutionLock = new Object()
        let solutionOnHoldLock = new Object()
        let intentionLock = new Object()
        let intentionIdLock = new Object()

        let generateIntentionId = lock intentionIdLock  (fun () -> intentionIdCounter <- intentionIdCounter + 1L
                                                                   intentionIdCounter)

        let rec travelDesires prio state (dTree:DesireTree<_,_>) = 
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

        
        
        let updateIntentions intenfilter currentInt (prio,intention) =
            let (conflics,harmonic) = Map.partition (fun  _ (_,i) ->  
                                                    let filter = intenfilter (i,intention)
                                                    match filter with
                                                    | Conflictive -> true
                                                    | Harmonic -> false
                                                    ) currentInt
            let highestPrio = Map.forall (fun _ (cp,_) -> prio > cp) conflics
            if highestPrio then
                let id = generateIntentionId
                Map.add id (prio,intention) harmonic
            else
                currentInt

        let buildIntentions intentionFilter state =
            lock intentionLock (fun () -> 
                let (_,newIntention) = travelDesires 0 state desires
                let parallelCalc = Array.Parallel.choose ( fun (p,ai) ->    match (ai state) with
                                                                            | Some i -> Some (p,i)
                                                                            | _ -> None )
                let newActualIntentions = List.ofArray ( parallelCalc (List.toArray newIntention) )
                let updatedIntentions = List.fold (updateIntentions intentionFilter) intentions newActualIntentions
                intentions <- updatedIntentions
            )
        

        let rec executePlan state intention id plan =
            if planner.SolutionFinished (state,intention,plan) then
                (true, plan)
            else
                let (action,rest) = planner.NextAction (state,intention,plan)
                let findActuator = List.tryFind (fun (act:Actuator<_>) -> act.CanPerformAction action ) actuators
                match findActuator with
                | Some actuator -> 
                    actuator.PerformAction action
                    executePlan state intention id rest
                | None ->
                    (false,plan)
            

        let attemptPromote id intention plan  =
            lock solutionsLock (fun () -> solutions <- Map.add id plan solutions)
            lock runningSolutionLock (fun () ->
                runningSolution <- Some id
                let curState = state
                
                let planWillWork = planner.PlanWorking (curState,intention,plan)
                let (planFinished,remaining) =  if planWillWork then
                                                    executePlan curState intention id plan
                                                else
                                                    false,plan
                runningSolution <- None
                if planFinished then
                    lock intentionLock (fun () -> intentions <- Map.remove id intentions)
                    lock solutionsLock (fun () -> solutions <- Map.remove id solutions )
                else
                    lock solutionsLock (fun () -> solutions <- Map.add id remaining solutions )
                    lock solutionOnHoldLock (fun () -> solutionsOnHold <- id :: solutionsOnHold)

                
            )
        

//        let checkActuators =
//            let curSolId = runningSolution
//            match curSolId with
//            | Some id -> 
//                let trySol = Map.tryFind id solutions
//                match trySol with
//                | Some sol -> 
//                    
//                | _ -> ()
//            | _ -> ()

//        let planReady id =
//            let startActuators = 
//                lock runningSolutionLock    
//                    (fun () -> 
//                        match runningSolution with
//                        | None -> runningSolution <- Some id
//                                  true
//                        | _ -> false
//                    )
//            if startActuators then
//                ()
        
//        let asyncIntentionPlanning state (id,intention) =
//            Async.Start 
//                (async
//                    {
//                        let plan = planner.FormulatePlan (state,intention)
//                        match plan with
//                        | Some sol -> 
//                            lock solutionLock (fun () -> solutions <- Map.add id plan solutions)
//                        | _ -> ()
//                    })

        member private this._newPercepts (sensor:Sensor<'TPercept>) = 
            let comp =  
                    async
                        {
                            let percepts = sensor.ReadPercepts()
                            lock stateLock (fun () ->  state <- List.fold (fun s p -> this.AnalyzePercept(p,s) ) state percepts)    
                            buildIntentions this.FilterIntention state

                        }
            
            Async.Start comp
            ()
        
        abstract member FilterIntention : ('TIntention*'TIntention) -> IntentionFilter
        abstract member AnalyzePercept : 'TPercept*'TState -> 'TState
        
       
           
        member this.AddSensor (sensor:Sensor<'TPercept>) = 
            sensors <- sensor :: sensors
            sensor.NewPercepts.Add(fun _ -> this._newPercepts(sensor))

        

        member this.AddAcuator (actuator:Actuator<'TAction>) =
            actuators <- actuator::actuators
            //actuator.

    end
    

