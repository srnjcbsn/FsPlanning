namespace FsPlanning.Agent
    open System;

    type Sensor<'TPercept> = 
        abstract member ReadPercepts : unit -> 'TPercept list
        [<CLIEvent>]
        abstract member NewPercepts : IEvent<EventHandler, EventArgs>