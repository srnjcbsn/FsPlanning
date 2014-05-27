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

            member this.Remove (cost, state) =
                new PriorityQueue<_,_> (Set.remove (cost, state) SortedSet, Map.remove state StateToCostMap)
            
            member this.TryPop () =
                if SortedSet.IsEmpty then
                    None
                else
                    let min = Set.minElement SortedSet
                    let newSet = Set.remove min SortedSet 
                    let newMap = Map.remove (snd min) StateToCostMap
                    Some (min, new PriorityQueue<_,_> (newSet, newMap))
            
            member this.Pop () =
                match this.TryPop() with
                | Some value -> value
                | _ -> failwith "Priority queue is empty"


            member this.PriorityOf (state) =
                Map.tryFind state StateToCostMap

            member this.Exists (state) =   
                Map.containsKey state StateToCostMap
                //Set.contains (prio,state) SortedSet

            member this.ToList () =
                Set.toList SortedSet

            new (elems : seq<'TPriority * 'TState>) =
                let SSet = Set.ofSeq elems
                let PMap = Map.ofSeq <| Seq.map (fun (a,b) -> (b,a)) elems
                new PriorityQueue<_,_> (SSet, PMap)
        end

    module PriorityQueue = 

        let add priority state (pq : PriorityQueue<_,_>) = pq.Add (priority, state)

        let remove priority state (pq : PriorityQueue<_,_>) = pq.Remove (priority, state)

        let pop (pq : PriorityQueue<_,_>) = pq.Pop ()

        let tryPop (pq : PriorityQueue<_,_>) = pq.TryPop ()

        let priorityOf state (pq : PriorityQueue<_,_>) = pq.PriorityOf (state)

        let exists state (pq : PriorityQueue<_,_>) = pq.Exists (state)

        let ofSeq elems = new PriorityQueue<_,_> (elems)

        let toList (pq : PriorityQueue<_,_>) = pq.ToList()

        let empty<'TState,'TPriority when 'TState : comparison and 'TPriority : comparison> = new PriorityQueue<_,_> ([])
    
        