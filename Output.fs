module Output
open Types
open SaveSystem
open OutputActor

let updateOutput (outputActor: OutputActor) worldState action =
    outputActor.Post (OutputMessage (Update {Action = action; WorldState = worldState}))
    match action with
    | CompletePlayerAction SaveGameAction ->
        let newOutputState = outputActor.PostAndReply OutputStateRequest
        saveGame "save.sav" worldState newOutputState
    | _ -> ()
