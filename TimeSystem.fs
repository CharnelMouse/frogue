module TimeSystem
open Types

let private rotate (lst: 'T list) =
    lst.Tail @ [lst.Head]

let updateTime combatState action = 
    match action with
        | CompleteAnyoneAction _ ->
            {combatState with
                ActorCombatQueue = rotate combatState.ActorCombatQueue
            }
        | _ ->
            combatState
