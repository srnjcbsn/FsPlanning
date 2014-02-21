namespace FsPlanning
module AStar =
    open FsPlanning.Searching
    
    val Solve : Problem<'a,'b> -> 'b list option when 'a : comparison
