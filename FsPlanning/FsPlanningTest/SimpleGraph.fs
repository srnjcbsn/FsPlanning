namespace FsPlanningTest
module SimpleGraph =
    
    let create : Set<_*_> = Set.empty

    
    let addEdge (n1,n2) graph = 
        let [a1, a2] = List.sort [n1, n2]
        Set.add (a1, a2) graph
    
    let ofList l = 
        let graph = create
        List.fold (fun g e -> addEdge e g) graph l


    let neighbours n graph =
        List.choose (fun edge -> 
                        match edge with
                        | (n1,n2) when n = n1 -> Some n2
                        | (n1,n2) when n = n2 -> Some n1
                        | _ -> None
                    ) <| Set.toList graph

