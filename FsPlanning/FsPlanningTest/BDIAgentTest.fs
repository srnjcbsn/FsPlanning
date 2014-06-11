namespace FsPlanningTest
module BDIAgentTest =
    
    open System
    open NUnit.Framework
    open FsPlanning.Search.Problem
    open FsPlanning.Search
    open FsPlanning.Agent
    open FsPlanning.Agent.Planning
    open FsPlanningTest.BDIMocks
   

    [<TestFixture>]
    type BDITest() = 
        


        [<Test>]
        member this.HandleIntention_TwoIntsConflicting_SequenceBoth () =
            let state = { HasSeen = [] }
            let makeIntention id state = if not <| List.exists (fun seen -> seen = id) state.HasSeen then Some <| IWantTo id else None

            let formulatePlan _ intent = 
                    match intent with
                    | IWantTo i -> Some [Do i]
                    | FailPlan -> None

            let desires = ManyDesires [ Desire (makeIntention 1); Desire (makeIntention 2)]

            let planner = new MockPlanner(formulatePlan)
            let actuSensor = new MockActuatorSensor()
            let agent = new MockBDIAgent(state,desires,planner,alwaysConflict)
            let asyncAwaitHandle = Async.AwaitEvent agent.FinishedIntentions
            agent.AddAcuator(actuSensor)
            agent.AddSensor(actuSensor);
            actuSensor.AddPercept WorldEvent
            ignore <| Async.RunSynchronously asyncAwaitHandle
            
            let resultState = agent.State
            let expectedState = { HasSeen = [2; 1] }

            Assert.AreEqual(expectedState, resultState)


        [<Test>]
        member this.HandleIntention_AtFirstUnplanableLaterPlanableIntention_IntentionExecuted() =
            let state = { HasSeen = [] }
            let intentId = 1
            let makeIntention id state = if not <| List.exists (fun seen -> seen = id) state.HasSeen then Some <| IWantTo id else None

            let desires = ManyDesires [ Desire (makeIntention 1); Desire (makeIntention 2)]

            let formulatePlan _ intent = 
                    match intent with
                    | IWantTo i when i = intentId && state.HasSeen.IsEmpty -> None
                    | IWantTo i -> Some [Do i]
                    | FailPlan -> None

            let planner = new MockPlanner(formulatePlan)
            let actuSensor = new MockActuatorSensor()
            let agent = new MockBDIAgent(state,desires,planner,alwaysConflict)
            let asyncAwaitHandle = Async.AwaitEvent agent.FinishedIntentions
            agent.AddAcuator(actuSensor)
            agent.AddSensor(actuSensor);
            actuSensor.AddPercept WorldEvent
            ignore <| Async.RunSynchronously asyncAwaitHandle
            
            let resultState = agent.State
            let expectedState = { HasSeen = [2; 1] }

            Assert.AreEqual(expectedState, resultState)