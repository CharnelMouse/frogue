open Types
open OutputActor
open DataConverter
open SaveSystem
open Action
open TimeSystem
open ActionGenerator

let private levelMap =
    CombatMap.create 20 10 (
        importMapTiles (String.concat "" [
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
    )

let private startingCombatState = {
    Actors = [
        0,
        {
            Tile = PlayerTile
            Controller = Player
            Name = "adventurer"
            Script = WaitScript
        }
        1,
        {
            Tile = OrcTile
            Controller = AIController
            Name = "orc"
            Script = DumbHunt
        }
        2,
        {
            Tile = OrcTile
            Controller = AIController
            Name = "orc"
            Script = DumbHunt
        }
        3,
        {
            Tile = OrcTile
            Controller = AIController
            Name = "orc"
            Script = DumbHunt
        }
        4,
        {
            Tile = OrcTile
            Controller = AIController
            Name = "orc"
            Script = DumbHunt
        }
    ]
    |> Map.ofList
    ActorCombatQueue = [0..4]
    ActorCombatPositions = [
        0, {X = 1; Y = 1}
        1, {X = 7; Y = 6}
        2, {X = 8; Y = 5}
        3, {X = 17; Y = 7}
        4, {X = 16; Y = 8}
    ]
    |> Map.ofList
    CombatMap = levelMap
}

let private startingTileset = DefaultTileset

let private startingStatusState = {
    StatusBar = {Start = {X = 0; Y = levelMap.Height + 1}; Length = 40}
    StatusBuffer = {Receiver = Player; Stream = ""}
}

let private startingAction = CompletePlayerAction StartSession

let rec private mainLoop (outputActor: OutputActor) combatState action =
    let newAction = generateAction combatState action
    let postExecuteCombat = executeAction combatState newAction
    outputActor.Post (Update {Action = newAction; CombatState = postExecuteCombat})
    outputActor.PostAndReply ReplyWhenReady
    match newAction with
    | CompletePlayerAction SaveGameAction ->
        let tileset, statusState = outputActor.PostAndReply OutputStateRequest
        saveGame "save.sav" postExecuteCombat tileset statusState
    | _ -> ()
    let newWorld = updateTime postExecuteCombat newAction
    let anyPlayerCombatActor =
        newWorld.Actors
        |> Map.exists (fun id {Controller = c} ->
            List.contains id newWorld.ActorCombatQueue && c = Player
            )
    match anyPlayerCombatActor, newAction with
    | false, _ ->
        outputActor.Post PushDie
        outputActor.Post (PopStatus {Reset = false; FullLinesOnly = false})
        outputActor.PostAndReply ReplyWhenReady
    | true, CompletePlayerAction QuitAction ->
        outputActor.Post (PopStatus {Reset = false; FullLinesOnly = false})
        outputActor.PostAndReply ReplyWhenReady
    | _ ->
        let currentActorID = newWorld.ActorCombatQueue.Head
        let currentActor =
            newWorld.Actors
            |> Map.find currentActorID
        outputActor.Post (PopStatusIfReceiverTurnOrFullLineInBuffer {Reset = true; CurrentActor = currentActor})
        outputActor.PostAndReply ReplyWhenReady
        mainLoop outputActor newWorld newAction

[<EntryPoint>]
let private main argv =
    let combatState, tileset, statusState, action =
        match tryLoadGame "save.sav" with
        | Some (cs, ts, ss, act) -> cs, ts, ss, act
        | None -> startingCombatState, startingTileset, startingStatusState, startingAction
    let outputActor = startOutputAgent tileset statusState
    outputActor.Post (Update {Action = action; CombatState = combatState})
    outputActor.Post (PopStatus {Reset = true; FullLinesOnly = false})
    mainLoop outputActor combatState action
    outputActor.PostAndReply ReplyWhenReady
    System.Console.ReadKey() |> ignore
    0 // return an integer exit code
