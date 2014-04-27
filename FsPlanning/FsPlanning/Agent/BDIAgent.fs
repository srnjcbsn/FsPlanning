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
        let mutable conflicts = Map.empty


        let mutable state = initstate
        let mutable actuators = []
        let mutable sensors = []

        

        //locks
        //let attemptPromotionLock = new Object()
        let stateLock = new Object()
        //let solutionsLock = new Object()
        //let runningSolutionLock = new Object()
        //let solutionOnHoldLock = new Object()
        let conflictLock = new Object()
        let intentionLock = new Object()
        let intentionIdLock = new Object()
        //let solvingLock = new Object()

        //Make a new ID meant for an intention
        let generateIntentionId = lock intentionIdLock  (fun () -> intentionIdCounter <- intentionIdCounter + 1L
                                                                   intentionIdCounter)
        //Takes an intention and checks if it conflicts with any of the other intentions 
        //if it has higher desire than the other intentions then 
        let updateIntentions intenfilter (currentInts,curConflicts) (prio,intention) =
            let (conflics,harmonic) = Map.partition (fun  _ (_,i,_) ->  
                                                    let filter = intenfilter (i,intention)
                                                    match filter with
                                                    | Conflictive -> true
                                                    | Harmonic -> false
                                                    ) currentInts
            let highestPrio = Map.forall (fun _ (cp,_,_) -> prio > cp) conflics
            
            let mappedConflicts = Map.ofSeq << Seq.map (fun (_,(desire,intent,_)) -> (desire,intent)) <| Map.toSeq conflics
            if highestPrio then
                let id = generateIntentionId
                let token = new CancellationTokenSource()
                Map.iter (fun _ (_,_,t:CancellationTokenSource) -> t.Cancel()) conflics
                (Map.add id (prio,intention,token) harmonic,Map.ofList ((Map.toList mappedConflicts)@(Map.toList curConflicts)))
            else
                (currentInts,Map.add prio intention curConflicts)

        
        let actionHandler act = 
            async
                {
                    let tryFindActu = List.tryFind (fun (actu:Actuator<_>) -> actu.CanPerformAction act ) actuators
                    match tryFindActu with
                    | Some actu ->
                        let running = ref true
                        while !running do
                            let actionPerformed = 
                                lock actu (fun () -> 
                                    if actu.IsReady then
                                        actu.PerformAction act
                                        if actu.IsReady then
                                            Some true
                                        else 
                                            Some false
                                    else
                                        None
                                    )
                            match actionPerformed with
                            | Some true -> running := false
                            | Some false -> 
                                let! _ = Async.AwaitEvent actu.ActuatorReady
                                running := false
                            | None -> 
                                let! _ = Async.AwaitEvent actu.ActuatorReady
                                ()  
                        return true
                    | None -> return false
                }

        let findNextAction intention plan =
            let s = lock stateLock (fun () -> state)
            let finished = planner.SolutionFinished (s, intention, plan)
            if not finished then
                let finalplan = planner.RepairPlan(s, intention, plan)    
                match finalplan with
                | Some p -> planner.NextAction (s, intention, p)
                | _ -> None
            else
                None
        let updateConflicts newCons =
            lock conflictLock (fun () -> conflicts <- Map.fold (fun cons desire inte -> Map.add desire inte cons) conflicts newCons )

        let updateAndStartIntentions intentionExecuter intentionFilter currentIntentions updatedIntentions =
              let (_,difIntents) = Map.partition (fun id _ -> Map.containsKey id currentIntentions) updatedIntentions
              intentions <- updatedIntentions
              Map.iter (fun id _ -> Async.Start <| intentionExecuter intentionFilter id) difIntents
        
        let rec intentionHandler filter id =
            async
                {
                    let pintent = lock intentionLock (fun () -> Map.tryFind id intentions)
                    match pintent with
                    | Some (_,intent,token:CancellationTokenSource) -> 
                        let s = lock stateLock (fun () -> state)
                        let planAttempt = planner.FormulatePlan (s, intent)
                        match planAttempt with
                        | Some plan ->
                            let curplan = ref plan
                            let running = ref true
                            while !running do
                                if not token.IsCancellationRequested then
                                    let actionAttempt = findNextAction intent !curplan
                                    match actionAttempt with
                                    | Some (act,rest) ->
                                        let! resolved = actionHandler act 
                                        if resolved then
                                            curplan := rest
                                        else
                                            running := false                                   
                                    | None -> running := false
                                else
                                    running:=false  
                            
                        | None -> ()

                        lock intentionLock (fun () ->   
                                                        intentions <- Map.remove id intentions
                                                        lock conflicts (fun () ->
                                                            let (newIntents,newCons) = List.fold (updateIntentions filter) (intentions,Map.empty) <| Map.toList conflicts
                                                            updateAndStartIntentions intentionHandler filter intentions newIntents
                                                            updateConflicts newCons 
                                                            ()            
                                                            ))

                    | _ -> ()
                }
        
        
        
        let buildIntentions intentionFilter state =
            let newCons = lock intentionLock (fun () -> 
                    let (_,newIntention) = travelDesires 0 state desires
                    let parallelCalc = Array.Parallel.choose ( fun (p,ai) ->    
                                                                                let calcIntention = ai state
                                                                                match calcIntention with
                                                                                | Some i -> Some (p,i)
                                                                                | _ -> None )
                    let newActualIntentions = List.ofArray ( parallelCalc (List.toArray newIntention) )
                    let currentIntentions = lock intentionLock (fun () -> intentions)
                    let (updatedIntentions,newConflicts) = List.fold (updateIntentions intentionFilter) (currentIntentions,Map.empty) newActualIntentions
//                    let (_,difIntents) = Map.partition (fun id _ -> Map.containsKey id currentIntentions) updatedIntentions
//                    intentions <- updatedIntentions
//                    Map.iter (fun id _ -> Async.Start <| intentionHandler intentionFilter id) difIntents
                    updateAndStartIntentions intentionHandler intentionFilter currentIntentions updatedIntentions
                    newConflicts
                )
            updateConflicts newCons 
        



        member private this._actuatorReady () = ()
//            let comp =
//                    async
//                        {
//                            ()
//                            //attemptPromote()
//                        }
//            Async.Start comp

        member private this._newPercepts (sensor:Sensor<'TPercept>) = 
            let percepts = sensor.ReadPercepts()
            let comp =  
                    async
                        {
                            lock stateLock (fun () ->  state <- this.AnalyzePercept(percepts,state) )   
                            buildIntentions this.FilterIntention state
                            //beginSolving planner state 
                            //attemptPromote()
                        }
            
            Async.Start comp
            ()
        
        abstract member FilterIntention : 'TIntention*'TIntention -> IntentionFilter
        abstract member AnalyzePercept : 'TPercept list*'TState -> 'TState
        
       
           
        member this.AddSensor (sensor:Sensor<'TPercept>) = 
            sensors <- sensor :: sensors
            sensor.NewPercepts.Add(fun _ -> this._newPercepts(sensor))

        

        member this.AddAcuator (actuator:Actuator<'TAction>) =
            actuators <- actuator::actuators
            actuator.ActuatorReady.Add(fun _ -> this._actuatorReady())
            //actuator.

    end
    

