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



        let mutable state = initstate
        let mutable actuators = []
        let mutable sensors = []

        

        //locks
        //let attemptPromotionLock = new Object()
        let stateLock = new Object()
        //let solutionsLock = new Object()
        //let runningSolutionLock = new Object()
        //let solutionOnHoldLock = new Object()
        let intentionLock = new Object()
        let intentionIdLock = new Object()
        //let solvingLock = new Object()

        //Make a new ID meant for an intention
        let generateIntentionId = lock intentionIdLock  (fun () -> intentionIdCounter <- intentionIdCounter + 1L
                                                                   intentionIdCounter)
        
        
        let actionHandler act = 
            async
                {
                    let tryFindActu = List.tryFind (fun (actu:Actuator<_>) -> actu.CanPerformAction act ) actuators
                    match tryFindActu with
                    | Some actu ->
                        let actionPerformed = ref false
                        while not(!actionPerformed) do
                            lock actu (fun () -> 
                                if actu.IsReady then
                                    actu.PerformAction act
                                    while not actu.IsReady do
                                        do! Async.Sleep(100)
                                    actionPerformed := true
                                ())
                            if not (!actionPerformed) then
                                do! Async.Sleep(100)
                        return true
                    | None -> return false
                }

        let findNextAction intention plan =
            let s = lock stateLock (fun () -> state)
            let finished = planner.SolutionFinished (s, intention, plan)
            if not finished then
                let finalplan = planner.RepairPlan(s, intention, plan)    
                match finalplan with
                | Some p -> Some <| planner.NextAction (s, intention, p)
                | _ -> None
            else
                None
        let intentionHandler id =
            async
                {
                    let pintent = lock intentionLock (fun () -> Map.tryFind id intentions)
                    match pintent with
                    | Some (_,intent) -> 
                        let s = lock stateLock (fun () -> state)
                        let planAttempt = planner.FormulatePlan (s, intent)
                        match planAttempt with
                        | Some plan ->
                            let curplan = ref plan
                            let running = ref true
                            while !running do
                                let actionAttempt = findNextAction intent !curplan
                                match actionAttempt with
                                | Some (act,rest) ->
                                    let! resolved = actionHandler act 
                                    if resolved then
                                        curplan := rest
                                    else
                                        running := false                                   
                                | None -> running := false  
                            
                        | None -> ()

                    | _ -> ()
                }
        
        //Takes an intention and checks if it conflicts with any of the other intentions 
        //if it has higher desire than the other intentions then 
        let updateIntentions intenfilter currentInts (prio,intention) =
            let (conflics,harmonic) = Map.partition (fun  _ (_,i) ->  
                                                    let filter = intenfilter (i,intention)
                                                    match filter with
                                                    | Conflictive -> true
                                                    | Harmonic -> false
                                                    ) currentInts
            let highestPrio = Map.forall (fun _ (cp,_) -> prio > cp) conflics
            if highestPrio then
                let id = generateIntentionId
                let token = Async.CancellationToken
                let t = 
                    async
                        {
                            
                            ()

                        }
                Map.add id (prio,intention) harmonic
            else
                currentInts
        
        let buildIntentions intentionFilter state =
            lock intentionLock (fun () -> 
                let (_,newIntention) = travelDesires 0 state desires
                let parallelCalc = Array.Parallel.choose ( fun (p,ai) ->    let calcIntention = ai state
                                                                            match calcIntention with
                                                                            | Some i -> Some (p,i)
                                                                            | _ -> None )
                let newActualIntentions = List.ofArray ( parallelCalc (List.toArray newIntention) )
                let updatedIntentions = List.fold (updateIntentions intentionFilter) intentions newActualIntentions
                lock intentionLock (fun () -> intentions <- updatedIntentions)
            )
        



        member private this._actuatorReady () =
            let comp =
                    async
                        {
                            ()
                            //attemptPromote()
                        }
            Async.Start comp

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
    

