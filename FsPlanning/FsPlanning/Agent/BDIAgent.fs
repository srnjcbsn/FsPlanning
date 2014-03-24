namespace FsPlanning.Agent
    

    [<AbstractClass>]
    type public BDIAgent<'TPercepts,'TState,'TAction when 'TState : comparison>(initstate) = class 
        
        let mutable beliefs:'TState = initstate
        let mutable actuators = []
        let mutable sensor = []

        abstract member AnalyzePercept : 'TPercept*'TState -> 'TState

        member this.AddSensor  (sensor:Sensor<_>,perceptWrapper) = 
            //sensor.NewPercepts.
            let input = sensor.ReadPercepts()
            
            ()

        

    end
    

