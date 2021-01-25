open Types
open Frogue
open OutputActor
open Output
open SaveSystem
open Status
open Action
open TimeSystem
open ActionGenerator

let private levelMap = Map.create 20 10 (importMapTiles [
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
    Map = levelMap
}

let private startingOutputState = {
    StatusBar = {Start = {X = 0; Y = levelMap.Height + 1}; Length = 40}
    StatusBuffer = {Receiver = Player; Stream = ""}
    Tileset = DefaultTileset
}

let private startingAction = CompletePlayerAction StartSession

let rec private mainLoop outputActor worldState action =
    let newAction = generateAction worldState action
    let postExecuteWorld = executeAction worldState newAction
    updateOutput outputActor postExecuteWorld newAction
    let newWorld = updateTime postExecuteWorld newAction
    let anyPlayerActor = List.tryFind (fun a -> a.Controller = Player) newWorld.Actors
    match anyPlayerActor, newAction with
    | None, _ ->
        outputActor.Post (OutputMessage PushDie)
        outputActor.Post (OutputMessage (PopStatus {Reset = false; FullLinesOnly = false}))
    | Some _, CompletePlayerAction QuitAction ->
        outputActor.Post (OutputMessage (PopStatus {Reset = false; FullLinesOnly = true}))
    | _ ->
        outputActor.Post (OutputMessage (PopStatusIfReceiverTurnOrFullLineInBuffer {Reset = true; CurrentActor = newWorld.Actors.Head}))
        mainLoop outputActor newWorld newAction

[<EntryPoint>]
let private main argv =
    let worldState, outputState, action =
        if saveGameExists "save.sav"
            then loadGame "save.sav"
            else startingWorldState, startingOutputState, startingAction
    let outputActor = startOutputAgent outputState
    updateOutput outputActor worldState action
    outputActor.Post (OutputMessage (PopStatus {Reset = true; FullLinesOnly = true}))
    mainLoop outputActor worldState action
    outputActor.PostAndReply ReplyWhenReady
    System.Console.ReadKey() |> ignore
    0 // return an integer exit code
