module TimeSystem
open Types
open ActionTypes

let private rotate lst =
    match lst with
    | [] -> []
    | [h] -> [h]
    | h :: t -> t @ [h]

let updateTime combatState action = 
    match action with
    | AnyoneAction _ ->
        {combatState with
            ActorCombatQueue = rotate combatState.ActorCombatQueue
        }
    | PlayerAction _ ->
        combatState
