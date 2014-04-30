namespace FsPlanning
    open System
    open System.Collections.Generic

    type PriorityQueue<'TPriority,'TState when 'TState : comparison and 'TPriority : comparison> 
        (sortedSet : Set<'TPriority * 'TState>, stateToCostMap : Map<'TState, 'TPriority>) =
        class
            let SortedSet = sortedSet
            let StateToCostMap = stateToCostMap

            member this.Add (cost, state) =
                new PriorityQueue<_,_> (Set.add (cost, state) SortedSet, Map.add state cost StateToCostMap)
            
            member this.Pop () =
                let min = Set.minElement SortedSet
                let newSet = Set.remove min SortedSet 
                let newMap = Map.remove (snd min) StateToCostMap
                (min, new PriorityQueue<_,_> (newSet, newMap))

            member this.PriorityOf (state) =
                Map.tryFind state StateToCostMap

            member this.Exists (state) =   
                Map.containsKey state StateToCostMap
                //Set.contains (prio,state) SortedSet

            new (elems : seq<'TPriority * 'TState>) =
                let SSet = Set.ofSeq elems
                let PMap = Map.ofSeq <| Seq.map (fun (a,b) -> (b,a)) elems
                new PriorityQueue<_,_> (SSet, PMap)
        end

    module PriorityQueue = 

        let add priority state (pq : PriorityQueue<_,_>) = pq.Add (priority, state)

        let pop (pq : PriorityQueue<_,_>) = pq.Pop ()

        let priorityOf state (pq : PriorityQueue<_,_>) = pq.PriorityOf (state)

        let exists state (pq : PriorityQueue<_,_>) = pq.Exists (state)

        let ofSeq elems = new PriorityQueue<_,_> (elems)

        let empty<'TState,'TPriority when 'TState : comparison and 'TPriority : comparison> = new PriorityQueue<_,_> ([])
    
