open Types
open ActionTypes
open DataConverter
open SaveSystem
open Status
open Output
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
            ControllerName = "player"
            Name = "adventurer"
            Script = WaitScript
        }
        1,
        {
            Tile = OrcTile
            ControllerName = "ai"
            Name = "orc"
            Script = DumbHunt
        }
        2,
        {
            Tile = OrcTile
            ControllerName = "ai"
            Name = "orc"
            Script = DumbHunt
        }
        3,
        {
            Tile = OrcTile
            ControllerName = "ai"
            Name = "orc"
            Script = DumbHunt
        }
        4,
        {
            Tile = OrcTile
            ControllerName = "ai"
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
    Controllers = [
        "player", Player
        "ai", AIController
    ]
    |> Map.ofList
}

let private startingTileset = DefaultTileset

let private startingStatusState = {
    StatusBar = {Start = {X = 0; Y = levelMap.Height + 1}; Length = 40}
    StatusBuffer = {Receiver = "player"; Stream = ""}
}

let rec private mainLoop tileset statusState game =
    match game with
    | Win actors ->
        actors
        |> List.countBy (fun a -> a.Name)
        |> List.fold (fun s (nm, c) -> s + " " + nm + ": " + string c) "You win! Remaining characters:"
        |> pushStatus statusState
        |> popStatus false false
        |> ignore
    | Combat combatState ->
        let action = generateAction combatState statusState
        let postExecuteCombat = executeAction combatState action
        let postUpdateTileset, postUpdateStatus = updateOutputState tileset statusState action postExecuteCombat
        match action with
        | PlayerAction SaveGameAction ->
            saveGame "save.sav" postExecuteCombat postUpdateTileset postUpdateStatus
        | _ ->
            ()
        let newCombat = updateTime postExecuteCombat action
        let playerCombatActors, nonPlayerCombatActors =
            newCombat.Actors
            |> Map.filter (fun id _ -> List.contains id newCombat.ActorCombatQueue)
            |> Map.partition (fun _ {ControllerName = cn} -> cn = "player")
        match Map.isEmpty playerCombatActors, Map.isEmpty nonPlayerCombatActors, action with
        | true, _, _ ->
            postUpdateStatus
            |> pushDieMessage
            |> popStatus false false
            |> ignore
        | false, true, _ ->
            let newStatus =
                postUpdateStatus
                |> popStatus false false
            let actorList =
                newCombat.Actors
                |> Map.toList
                |> List.map (fun (_, a) -> a)
            mainLoop postUpdateTileset newStatus (Win actorList)
        | false, false, PlayerAction QuitAction ->
            postUpdateStatus
            |> popStatus false false
            |> ignore
        | false, false, _ ->
            let currentActorID = newCombat.ActorCombatQueue.Head
            let currentActor =
                newCombat.Actors
                |> Map.find currentActorID
            let newStatusState =
                postUpdateStatus
                |> popStatusIfReceiverTurnOrFullLineInBuffer true currentActor
            mainLoop postUpdateTileset newStatusState (Combat newCombat)

[<EntryPoint>]
let private main argv =
    let combatState, tileset, statusState, startResult =
        match tryLoadGame "save.sav" with
        | Some (cs, ts, ss, res) -> cs, ts, ss, res
        | None -> startingCombatState, startingTileset, startingStatusState, NormalStart
    let newTileset, newStatus = startOutput tileset statusState startResult combatState
    let newNewStatus =
        newStatus
        |> popStatus true false
    mainLoop newTileset newNewStatus (Combat combatState)
    System.Console.ReadKey() |> ignore
    0 // return an integer exit code
