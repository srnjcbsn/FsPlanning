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

//        let mutable planning = Set.empty
//        let mutable onHold = Set.empty
//        let mutable executing = Set.empty


        let mutable state = initstate
        let mutable actuators = []
        let mutable sensors = []

        

        //locks
        //let attemptPromotionLock = new Object()
        let stateLock = new Object()
        //let solutionsLock = new Object()
        //let runningSolutionLock = new Object()
        //let solutionOnHoldLock = new Object()
        //let conflictLock = new Object()
        let intentionLock = new Object()
        let intentionIdLock = new Object()
        //let solvingLock = new Object()

        //Make a new ID meant for an intention
        let generateIntentionId () = lock intentionIdLock  (fun () -> intentionIdCounter <- intentionIdCounter + 1L
                                                                      intentionIdCounter)

//        let isIntentionOnHold id =
//            lock intentionLock (fun () -> Set.contains id onHold)

        let finishedIntentionsEvent = new Event<EventHandler, EventArgs>()

//        //The 3 modes intentions can be in Onhold, Planning and Executing
//        let setIntentionAsOnHold id = 
//            lock intentionLock (fun () -> 
//                onHold <- Set.add id onHold
//                planning <- Set.remove id planning
//                executing <- Set.remove id executing)
//
//        let setIntentionAsPlanning id = 
//            lock intentionLock (fun () -> 
//                onHold <- Set.remove id onHold
//                planning <- Set.add id planning
//                executing <- Set.remove id executing)
//        
//        let setIntentionAsExecuting id = 
//            lock intentionLock (fun () -> 
//                onHold <- Set.remove id onHold
//                planning <- Set.remove id planning
//                executing <- Set.add id executing)
//
//        //Removes intention from the agent
//        let removeIntention id =
//            lock intentionLock 
//                (fun () ->
//                    onHold <- Set.remove id onHold
//                    planning <- Set.remove id planning
//                    executing <- Set.remove id executing
//                    intentions <- Map.remove id intentions
//                )

        //Takes an intention and checks if it conflicts with any of the other intentions 
        //if it has higher desire than the other intentions then 
        let updateIntentions (intentionEqual,intenfilter) (currentInts,curConflicts) (prio,intention) =
            //printf "%A Intentions: %A\n" (List.length <| Map.toList intentions) (List.map snd <| Map.toList intentions)
            let (conflictingInts,harmonicInts) = Map.partition (fun  _ (_,i,_) ->  
                                                    let filter = intenfilter (i,intention)
                                                    match filter with
                                                    | Conflictive -> true
                                                    | Harmonic -> false
                                                    ) currentInts

            let highestPrio = Map.forall (fun _ (cp,_,_) -> prio < cp) conflictingInts
            
            
                
            if highestPrio then
                let mappedConflicts = Map.ofList <| ( List.map (fun (_,(desire,intent,_)) -> (desire,intent)) <| Map.toList conflictingInts )
                let id = generateIntentionId()
                let token = new CancellationTokenSource()
                Map.iter (fun _ (_,_,t:CancellationTokenSource) -> t.Cancel()) conflictingInts
                let newIntentions = Map.add id (prio,intention,token) harmonicInts
                let newConflicts = Map.ofList ((Map.toList mappedConflicts)@(Map.toList curConflicts))
                (newIntentions,newConflicts)
            else
                let allcurrentCons = List.map (fun (_,i) -> i) <| Map.toList curConflicts
                let alreadyExists = List.exists (fun i -> intentionEqual (i, intention)) <| allcurrentCons
                if not alreadyExists then
                    (currentInts,Map.add prio (intention) curConflicts)
                else
                    (currentInts,curConflicts) 

        
        let actionHandler act = 
            async
                {
                    let tryFindActu = List.tryFind (fun (actu:Actuator<_>) -> actu.CanPerformAction act ) actuators
                    match tryFindActu with
                    | Some actu ->
                        lock actu (fun () -> actu.PerformActionBlockUntilFinished act)
                        return true
                    | None -> return false
                }

        let findNextAction intention plan =
            let s = lock stateLock (fun () -> state)
            let finished = planner.SolutionFinished (s, intention, plan)
            if not finished then
                let newState = lock stateLock (fun () -> state <- planner.UpdateStateBeforePlanning(state,intention)
                                                         state)
                let finalplan = planner.RepairPlan(newState, intention, plan)    
                match finalplan with
                | Some p -> 
                    match planner.NextAction (newState, intention, p) with
                    | Some action -> Choice2Of2 action
                    | None -> Choice1Of2 false
                | _ -> Choice1Of2 false
            else
                lock stateLock (fun () -> state <- planner.UpdateStateOnSolutionFinished(state,intention,plan))
                Choice1Of2 true
        let updateConflicts newCons =
            lock intentionLock (fun () -> conflicts <-  newCons )//Map.fold (fun cons desire inte -> Map.add desire inte cons) conflicts newCons )

        let updateAndStartIntentions intentionExecuter intentionFilter currentIntentions updatedIntentions =
              let (_,difIntents) = Map.partition (fun id _ -> Map.containsKey id currentIntentions) updatedIntentions
              lock intentionLock (fun () -> intentions <- updatedIntentions)
              Map.iter (fun id _ -> Async.Start <| intentionExecuter intentionFilter id) difIntents
        
        let rec planHandler intent (token:CancellationTokenSource) plan =
            async
                {
                    if not token.IsCancellationRequested then
                        let actionAttempt = findNextAction intent plan
                        match actionAttempt with
                        | Choice1Of2 success -> return success
                        | Choice2Of2 (act,rest) ->
                            let! resolved = actionHandler act
                            return! planHandler intent token rest
                    else
                        return false
                }
        

        
        let rec intentionHandler finishedTrigger filter id =
            async
                {
                    
                    let pintent = lock intentionLock (fun () -> Map.tryFind id intentions)
                    //printf "Beginning: %A" pintent
                    match pintent with
                    | Some (_,intent,token:CancellationTokenSource) -> 
                        let s = lock stateLock (fun () -> state <- planner.UpdateStateBeforePlanning(state,intent)
                                                          state)
                        let planAttempt =  try planner.FormulatePlan (s, intent) with
                                           | exn -> printfn "Exception encountered in planning: %A \n %A at %A" intent exn.Message exn.TargetSite
                                                    None
                        match planAttempt with
                        | Some plan ->
                            let! success = planHandler intent token plan
                            ()                        
                        | None -> 
                            ()

                        lock intentionLock 
                                (fun () ->
                                    intentions <- Map.remove id intentions
                                    let (newIntents,newCons) = List.fold (updateIntentions filter) (intentions,Map.empty) <| Map.toList conflicts
                                    updateAndStartIntentions (intentionHandler finishedTrigger) filter intentions newIntents 
                                    updateConflicts newCons
                                    if intentions.Count = 0 then
                                        finishedTrigger()    
                                )

                    | _ -> ()
                }
        
        
        
        let buildIntentions finishedTrigger intentionFilter state =
            let newCons = lock intentionLock (fun () -> 
                    let (_,newIntention) = travelDesires 0 state desires
                    let parallelCalc = 
                        Array.Parallel.choose 
                            ( 
                                fun (p,ai) ->    
                                try
                                    let calcIntention = ai state
                                    match calcIntention with
                                    | Some i -> Some (p,i)
                                    | _ -> None
                                with
                                | e ->  printf "Intention Function crash: \n%A \n%A" ai e
                                        None
                            )
                    let newActualIntentions = List.ofArray ( parallelCalc (List.toArray newIntention) )
                    let currentIntentions = lock intentionLock (fun () -> intentions)
                    let (updatedIntentions,newConflicts) = List.fold (updateIntentions intentionFilter) (currentIntentions,Map.empty) newActualIntentions
//                    let (_,difIntents) = Map.partition (fun id _ -> Map.containsKey id currentIntentions) updatedIntentions
//                    intentions <- updatedIntentions
//                    Map.iter (fun id _ -> Async.Start <| intentionHandler intentionFilter id) difIntents
                    updateAndStartIntentions (intentionHandler finishedTrigger) intentionFilter currentIntentions updatedIntentions
                    newConflicts
                    
                )
            updateConflicts newCons 
        
        member private this._triggerFinishedIntentions () = finishedIntentionsEvent.Trigger(this, new EventArgs())

        [<CLIEvent>]
        member this.FinishedIntentions = finishedIntentionsEvent.Publish

        member this.State = lock stateLock (fun () -> state)
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
            let newState = lock stateLock (fun () ->  state <- this.AnalyzePercept(percepts,state)
                                                      state )
            let optimize = async
                                {
                                    let optState  = this.OptimizeState(newState)
                                    lock stateLock (fun () ->  state <- this.ImplementOptimizedState(state,optState))
                                    ()
                                }
            Async.Start(optimize)
            buildIntentions (this._triggerFinishedIntentions) (this.IsIntentionEqual,this.FilterIntention) state
        
        abstract member FilterIntention : 'TIntention*'TIntention -> IntentionFilter
        abstract member AnalyzePercept : 'TPercept list*'TState -> 'TState
        abstract member OptimizeState  : 'TState -> 'TState
        abstract member ImplementOptimizedState : 'TState*'TState -> 'TState
        abstract member IsIntentionEqual : 'TIntention*'TIntention -> bool

        member this.AddSensor (sensor:Sensor<'TPercept>) = 
            sensors <- sensor :: sensors
            sensor.NewPercepts.Add(fun _ -> this._newPercepts(sensor))

        

        member this.AddAcuator (actuator:Actuator<'TAction>) =
            actuators <- actuator::actuators
            actuator.ActuatorReady.Add(fun _ -> this._actuatorReady())
            //actuator.

    end
    

