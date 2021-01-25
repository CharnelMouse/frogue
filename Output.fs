module Output
open Types
open FileActor
open OutputActor

let updateOutput (fileActor: FileActor) (outputActor: OutputActor) worldState action =
    outputActor.Post (OutputMessage (Update {Action = action; WorldState = worldState}))
    match action with
    | CompletePlayerAction SaveGameAction ->
        let outputState = outputActor.PostAndReply OutputStateRequest
        fileActor.Post (SaveGameMessage {WorldState = worldState; OutputState = outputState})
    | _ -> ()
