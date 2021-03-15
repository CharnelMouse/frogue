module TimeSystem
open Types

let private rotate (lst: 'T list) =
    lst.Tail @ [lst.Head]

let updateTime worldState action = 
    match action with
        | CompleteAnyoneAction _ ->
            {worldState with
                ActorCombatQueue = rotate worldState.ActorCombatQueue
            }
        | _ ->
            worldState
