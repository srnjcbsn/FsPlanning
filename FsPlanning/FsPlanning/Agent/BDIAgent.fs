namespace FsPlanning.Agent

    open System
    open System.Threading;
    open Planning

    [<AbstractClass>]
    type public BDIAgent<'TPercept,'TState,'TAction,'TIntention,'TGoal when 'TState : comparison>(initstate,desires) = class 
        
        let desires:DesireTree<'TState,'TIntention*'TGoal> = desires
        let mutable intentions = []
        let mutable state = initstate
        let mutable actuators = []
        let mutable sensors = []

        let stateLock = new Object()


        let rec travelDesires prio state (dTree:DesireTree<_,_*_>) = 
            match dTree with
            | Conditional (c,t) ->
                let lastprio,goals = travelDesires prio state t
                if c(state) then
                    (lastprio,goals)
                else
                    (lastprio,[])
            | Desire cG ->
                let optGoal = cG(state)
                let lp = prio + 1
                match optGoal with
                | Some goal -> 
                    (lp,[(lp,goal)])
                | _ -> (lp,[])
                   
            | ManyDesires tree -> 
                List.fold   (fun (p,ds) t -> 
                                    let (newp,newDs) = travelDesires p state t
                                    (newp,ds@newDs)
                            ) (prio,[]) tree 

        
        
        let updateIntentions intenfilter currentInt (prio,intention) =
            let (conflics,_) = List.partition (fun (_,i) ->  
                                                    let filter = intenfilter (i,intention)
                                                    match filter with
                                                    | Conflictive -> true
                                                    | Harmonic -> false
                                                    ) currentInt
            
            currentInt

        let decideGoal intentionFilter state =
            let (_,intention) = travelDesires 0 state desires
            let intentions = List.fold (updateIntentions intentionFilter) intentions intention


            () 
        

        member private this._newPercepts (sensor:Sensor<'TPercept>) = 
            let comp =  
                    async
                        {
                            let percepts = sensor.ReadPercepts()
                            lock stateLock (fun () ->  state <- List.fold (fun s p -> this.AnalyzePercept(p,s) ) state percepts)    
                               
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
        

    end
    

