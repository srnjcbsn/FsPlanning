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
        let mutable attemptedPromotions = 0

        //locks
        let attemptPromotionLock = new Object()
        let stateLock = new Object()
        let solutionsLock = new Object()
        let runningSolutionLock = new Object()
        let solutionOnHoldLock = new Object()
        let intentionLock = new Object()
        let intentionIdLock = new Object()

        let generateIntentionId = lock intentionIdLock  (fun () -> intentionIdCounter <- intentionIdCounter + 1L
                                                                   intentionIdCounter)

        let rec travelDesires prio state (dTree:DesireTree<'TState,'TIntention>) = 
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
                let parallelCalc = Array.Parallel.choose ( fun (p,ai) ->    let calcIntention = ai state
                                                                            match calcIntention with
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
        
        

        let intentionsWithSolutions =
            let tupToTrip ((a,b),c) = (a,b,c) 
            lock solutionsLock (fun () ->
                lock intentionLock (fun () ->
                    Map.map (fun id sol -> tupToTrip (Map.find id intentions,sol) ) solutions))
        let executePromotedPlan id intention plan =
            lock solutionOnHoldLock (fun () -> solutionsOnHold <- List.filter (fun sId -> sId <> id) solutionsOnHold)
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

        let attemptPromote()  =
            let shouldPromote = lock attemptPromotionLock (fun () ->    if attemptedPromotions >= 2 then
                                                                            false
                                                                        else
                                                                            attemptedPromotions <- attemptedPromotions + 1
                                                                            true)
            if shouldPromote then
                lock runningSolutionLock (fun () ->
                    let possibePlans = Map.toList intentionsWithSolutions
                    match possibePlans with
                    | [] -> ()
                    | plans ->
                        let (id,(_,intent,plan)) = List.minBy (fun (_,(desire,_,_)) -> desire) plans
                        lock runningSolutionLock (fun () -> executePromotedPlan id intent plan)             

                )
                lock attemptPromotionLock (fun () -> attemptedPromotions <- attemptedPromotions - 1)
            else
                ()
        
        
        member private this._actuatorReady () =
            let comp =
                    async
                        {
                            attemptPromote()
                        }
            Async.Start comp

        member private this._newPercepts (sensor:Sensor<'TPercept>) = 
            let percepts = sensor.ReadPercepts()
            let comp =  
                    async
                        {
                            lock stateLock (fun () ->  state <- List.fold (fun s p -> this.AnalyzePercept(p,s) ) state percepts)    
                            buildIntentions this.FilterIntention state
                            attemptPromote()
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
            actuator.ActuatorReady.Add(fun _ -> this._actuatorReady())
            //actuator.

    end
    

