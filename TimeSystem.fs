module TimeSystem
open Types

let private rotate (actors: 'T list) =
    actors.Tail @ [actors.Head]

let updateTime worldState action = 
    match action with
        | CompleteAnyoneAction _ -> {worldState with Actors = rotate worldState.Actors}
        | _ -> worldState
