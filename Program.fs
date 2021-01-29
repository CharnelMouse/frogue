open Types
open OutputActor
open DataConverter
open FileActor
open Action
open TimeSystem
open ActionGenerator

let private levelMap = CombatMap.create 20 10 (importMapTiles [
    "####################"
    "#              +    "
    "#              #   #"
    "#              #   #"
    "#              #   #"
    "#              #   #"
    "#              #   #"
    "#              #   #"
    "#              #   #"
    "####################"
])

let private startingWorldState = {
    Actors = [
        {
            Position = {X = 1; Y = 1}
            Tile = PlayerTile
            Controller = Player
            Name = "adventurer"
            Script = WaitScript
        }
        {
            Position = {X = 7; Y = 6}
            Tile = OrcTile
            Controller = AIController
            Name = "orc"
            Script = DumbHunt
        }
        {
            Position = {X = 8; Y = 5}
            Tile = OrcTile
            Controller = AIController
            Name = "orc"
            Script = DumbHunt
        }
        {
            Position = {X = 17; Y = 7}
            Tile = OrcTile
            Controller = AIController
            Name = "orc"
            Script = DumbHunt
        }
        {
            Position = {X = 16; Y = 8}
            Tile = OrcTile
            Controller = AIController
            Name = "orc"
            Script = DumbHunt
        }
    ]
    CombatMap = levelMap
}

let private startingOutputState = {
    StatusBar = {Start = {X = 0; Y = levelMap.Height + 1}; Length = 40}
    StatusBuffer = {Receiver = Player; Stream = ""}
    Tileset = DefaultTileset
}

let private startingAction = CompletePlayerAction StartSession

let rec private mainLoop (fileActor: FileActor) (outputActor: OutputActor) worldState action =
    let newAction = generateAction worldState action
    let postExecuteWorld = executeAction worldState newAction
    outputActor.Post (Update {Action = newAction; WorldState = postExecuteWorld})
    match newAction with
    | CompletePlayerAction SaveGameAction ->
        let outputState = outputActor.PostAndReply OutputStateRequest
        fileActor.Post (SaveGameMessage {WorldState = postExecuteWorld; OutputState = outputState})
    | _ -> ()
    let newWorld = updateTime postExecuteWorld newAction
    let anyPlayerActor = List.tryFind (fun a -> a.Controller = Player) newWorld.Actors
    match anyPlayerActor, newAction with
    | None, _ ->
        outputActor.Post PushDie
        outputActor.Post (PopStatus {Reset = false; FullLinesOnly = false})
    | Some _, CompletePlayerAction QuitAction ->
        outputActor.Post (PopStatus {Reset = false; FullLinesOnly = true})
    | _ ->
        outputActor.Post (PopStatusIfReceiverTurnOrFullLineInBuffer {Reset = true; CurrentActor = newWorld.Actors.Head})
        mainLoop fileActor outputActor newWorld newAction

[<EntryPoint>]
let private main argv =
    let fileActor = startFileAgent "save.sav"
    let worldState, outputState, action =
        match fileActor.PostAndReply LoadGameRequest with
        | Some (ws, os, act) -> ws, os, act
        | None -> startingWorldState, startingOutputState, startingAction
    let outputActor = startOutputAgent outputState
    outputActor.Post (Update {Action = action; WorldState = worldState})
    outputActor.Post (PopStatus {Reset = true; FullLinesOnly = true})
    mainLoop fileActor outputActor worldState action
    outputActor.PostAndReply ReplyWhenReady
    System.Console.ReadKey() |> ignore
    0 // return an integer exit code
