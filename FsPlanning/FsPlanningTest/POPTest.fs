module POPTest
    
    open NUnit.Framework;
    open FsPlanning.Planning.POP.Types
    open FsPlanning.Planning.POP.Action
    open FsPlanning.Planning.POP
    [<TestFixture>]
    type POPTest() = 

            
            
            [<Test>]
            member this.FindSolution_PaintProblem_FindCorrectSolution () =
                let CanPaint = Predicate "CanPaint"
                let Can = Predicate "Can"
                let Box = Predicate "Box"
                let Painted = Predicate "Painted" 
                let paramOC = Parameter "oc"
                let paramC = Parameter "c"
                let paramNC = Parameter "nc"
                let varOC = Parameter.VariableOf paramOC
                let varC = Parameter.VariableOf paramC
                let varNC = Parameter.VariableOf paramNC
                let t = NotEqual (varOC, varNC)
                let dipPrecond = [Pos (CanPaint [varOC]); Pos (Can [varC; varNC]); NotEqual (varOC, varNC)] 
                let dipEffect = [Neg <| CanPaint [varOC]; Pos <| CanPaint [varNC]]
                let actDip = Action "Dip" [paramOC; paramC; paramNC] dipPrecond dipEffect

                let paramB = Parameter "b"
                let varB = Parameter.VariableOf paramB
                let paintPrecond = AllPosPredicates [Box [varB]; Painted [varB; varOC]; CanPaint  [varNC]]
                let paintEffect = [Neg <| Painted [varB; varOC]; Pos <| Painted [varB; varNC]]
                let actPaint = Action "Paint" [paramB; paramOC; paramNC] paintPrecond paintEffect

                let B1 = Constant "B1"
                let B2 = Constant "B2"
                let B3 = Constant "B3"

                let C1 = Constant "C1"
                let C2 = Constant "C2"

                let RED = Constant "Red"
                let BLUE = Constant "Blue"
                let GREEN = Constant "Green"

                let goalAct = Action "goal" [] (AllPosPredicates [Painted [B1; RED]; Painted [B2; RED]; Painted [B3; BLUE]]) []

                let initAct = Action "init" [] [] (AllPosPredicates [Can [C1; RED]; Can [C2; BLUE]; Painted [B1; GREEN];  Painted [B2; GREEN];  Painted [B3; GREEN]; CanPaint [GREEN] ])

                let solution = Plan.solve [actDip; actPaint] initAct goalAct 

                ()