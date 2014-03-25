namespace FsPlanning.Agent
    
    open System

    type Actuator<'TAction> = 
        abstract member CanPerformAction : 'TAction -> bool
        abstract member PerformAction : 'TAction -> unit
        abstract member IsReady : bool
        [<CLIEvent>]
        abstract member ActuatorReady : IEvent<EventHandler, EventArgs>

