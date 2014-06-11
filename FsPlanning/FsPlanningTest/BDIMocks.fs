namespace FsPlanningTest
module BDIMocks =

    open System
    open NUnit.Framework
    open FsPlanning.Search.Problem
    open FsPlanning.Search
    open FsPlanning.Agent
    open FsPlanning.Agent.Planning

    type Percept =
        | Seen of int
        | WorldEvent

    type Action =
        | Do of int

    type State =
        {
            HasSeen : int list
        }
    
    type Intention = 
        | IWantTo of int
        | FailPlan

    type Plan = Action list
    
    type MockPlanner(formulatePlan) =
        class
            interface Planner<State, Action, Intention, Plan> with 
                member self.FormulatePlan (state, intent) = formulatePlan state intent

                member self.RepairPlan (state, intent, solution) = Some solution

                member self.SolutionFinished (state, intent, solution) = 
                    match solution with
                    | [] -> 
                        true
                            
                    | _ -> false
                member self.NextAction (state, intent, solution) = 
                    match solution with
                    | act::rest -> Some (act,rest)
                    | [] -> None

                member self.UpdateStateBeforePlanning (state, intent) = state
                member self.UpdateStateOnSolutionFinished (state, intent, solution) = state
        end

    type MockBDIAgent(State,DesireTree,Planner,filter) =
        class
           
            inherit BDIAgent<Percept,State,Action,Intention,Plan>(State,DesireTree,Planner)
                override this.AnalyzePercept(percepts, state) = 
                    List.fold   (fun s p -> 
                                    match p with
                                    | Seen i -> 
                                        { s with HasSeen = i::s.HasSeen }
                                    | WorldEvent ->
                                        s
                                ) state percepts
            
                override this.FilterIntention(intA, intB) = filter intA intB
                
                override this.IsIntentionEqual (intA,intB) = intA = intB
                override this.OptimizeState(curState) = curState
                override this.ImplementOptimizedState(curState,optState)= curState
        end
   
    type MockActuatorSensor() =
        
        let NewPerceptsEvent = new Event<EventHandler, EventArgs>()
        let ActuatorReadyEvent = new Event<EventHandler, EventArgs>()
        let perceptLock = new Object()
        let mutable awaitingPercepts = []

        member this.AddPercept percept =
            lock perceptLock (fun () -> awaitingPercepts <- percept::awaitingPercepts)
            NewPerceptsEvent.Trigger(this, new EventArgs())

        interface Actuator<Action> with
                member this.CanPerformAction action = true
                

                member this.PerformAction action = 
                    let percept =
                        match action with
                        | Do id -> Seen id

                    lock perceptLock (fun () -> awaitingPercepts <- percept::awaitingPercepts)
                    NewPerceptsEvent.Trigger(this, new EventArgs())
                
                
                member this.PerformActionBlockUntilFinished action = (this :> Actuator<Action>).PerformAction action

                [<CLIEvent>]
                member this.ActuatorReady = ActuatorReadyEvent.Publish
                member this.IsReady =  true
        interface Sensor<Percept> with
            member this.ReadPercepts() = 
                lock perceptLock (fun () ->
                    let percepts = awaitingPercepts
                    awaitingPercepts <- []
                    percepts)
            [<CLIEvent>]
            member this.NewPercepts = NewPerceptsEvent.Publish
    
    let alwaysConflict _ _ = Conflictive
    let spawnInt intent _ = Some intent 

