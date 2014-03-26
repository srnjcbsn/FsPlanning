namespace FsPlanning.Agent

    open System
    open System.Threading;
    open Planning

    [<AbstractClass>]
    type public BDIAgent<'TPercept,'TState,'TAction,'TIntention when 'TState : comparison>(initstate,desires,planner) = class 
        
        let desires:DesireTree<'TState,'TIntention> = desires
        let planner:Planner<'TState, 'TAction,'TIntention> = planner
        let mutable intentions = []
        let mutable state = initstate
        let mutable actuators = []
        let mutable sensors = []

        let stateLock = new Object()
        let intentionLock = new Object()

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
            let (conflics,harmonic) = List.partition (fun (_,i) ->  
                                                    let filter = intenfilter (i,intention)
                                                    match filter with
                                                    | Conflictive -> true
                                                    | Harmonic -> false
                                                    ) currentInt
            let highestPrio = List.forall (fun (cp,_) -> prio > cp) conflics
            if highestPrio then
                (prio,intention)::harmonic
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
    

