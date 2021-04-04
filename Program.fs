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
            ControllerName = "orcs"
            Name = "orc"
            Script = DumbHunt
        }
        2,
        {
            Tile = OrcTile
            ControllerName = "orcs"
            Name = "orc"
            Script = DumbHunt
        }
        3,
        {
            Tile = OrcTile
            ControllerName = "orcs"
            Name = "orc"
            Script = DumbHunt
        }
        4,
        {
            Tile = OrcTile
            ControllerName = "orcs"
            Name = "orc"
            Script = DumbHunt
        }
        5,
        {
            Tile = PlayerTile
            ControllerName = "party"
            Name = "adventurer"
            Script = DumbHunt
        }
        6,
        {
            Tile = TrollTile
            ControllerName = "trolls"
            Name = "troll"
            Script = DumbHunt
        }
        7,
        {
            Tile = OgreTile
            ControllerName = "ogres"
            Name = "ogre"
            Script = DumbHunt
        }
    ]
    |> Map.ofList
    ActorCombatQueue = [0..7]
    ActorCombatPositions = [
        0, {X = 1; Y = 1}
        1, {X = 7; Y = 6}
        2, {X = 8; Y = 5}
        3, {X = 17; Y = 7}
        4, {X = 16; Y = 8}
        5, {X = 2; Y = 1}
        6, {X = 17; Y = 5}
        7, {X = 14; Y = 2}
    ]
    |> Map.ofList
    CombatMap = levelMap
    Controllers = [
        "player", Player
        "orcs", AIController
        "party", AIController
        "trolls", AIController
        "ogres", AIController
    ]
    |> Map.ofList
    ControllerRelations = [
        ("player", "player"), SameController
        ("player", "orcs"), Enemy
        ("player", "party"), Ally
        ("player", "trolls"), Enemy
        ("player", "ogres"), Enemy
        ("orcs", "player"), Enemy
        ("orcs", "orcs"), SameController
        ("orcs", "party"), Enemy
        ("orcs", "trolls"), Ally
        ("orcs", "ogres"), Enemy
        ("party", "player"), Ally
        ("party", "orcs"), Enemy
        ("party", "party"), SameController
        ("party", "trolls"), Enemy
        ("party", "ogres"), Enemy
        ("trolls", "player"), Enemy
        ("trolls", "orcs"), Ally
        ("trolls", "party"), Enemy
        ("trolls", "trolls"), SameController
        ("trolls", "ogres"), Enemy
        ("ogres", "player"), Enemy
        ("ogres", "orcs"), Enemy
        ("ogres", "party"), Enemy
        ("ogres", "trolls"), Enemy
        ("ogres", "ogres"), SameController
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
        let enemyControllerNames =
            postExecuteCombat.ControllerRelations
            |> Map.filter (fun (c, _) r -> c = "player" && r = Enemy)
            |> Map.toList
            |> List.map (fun ((_, c), _) -> c)
        let playerCombatActors, nonPlayerCombatActors =
            newCombat.Actors
            |> Map.filter (fun id _ -> List.contains id newCombat.ActorCombatQueue)
            |> Map.partition (fun _ {ControllerName = cn} -> cn = "player")
        let enemyCombatActors =
            nonPlayerCombatActors
            |> Map.filter (fun _ {ControllerName = cn} -> List.contains cn enemyControllerNames)
        match Map.isEmpty playerCombatActors, Map.isEmpty enemyCombatActors, action with
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
