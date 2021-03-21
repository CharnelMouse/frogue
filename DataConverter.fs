module DataConverter
open Types
open Tilesets

let private exportActor (id, actor) =
    let controller =
        match actor.Controller with
        | Player -> "player"
        | AIController -> "ai"
    let name = actor.Name
    let script =
        match actor.Script with
        | WaitScript -> "waitAI"
        | StandGround -> "standgroundAI"
        | DumbHunt -> "dumbhuntAI"
    [
        string id
        name
        string (defaultTilesetParser.CombatActorParser actor.Tile)
        controller
        script
        ]
    |> List.toSeq
    |> String.concat ";"

let private importActor (str: string) =
    let vals = Array.toList (str.Split ";")
    match vals with
    | [id; name; tile; controller; script]  ->
        int id,
        {
            Tile = getActorTile (char tile)
            Controller =
                match controller with
                | "player" -> Player
                | "ai" -> AIController
                | _ -> failwith ("invalid actor: unrecognised controller: " + controller)
            Name = name
            Script =
                match script with
                | "waitAI" -> WaitScript
                | "standgroundAI" -> StandGround
                | "dumbhuntAI" -> DumbHunt
                | _ -> failwith ("invalid actor: unrecognised AI: " + script)
        }
    | _ -> failwith ("invalid actor: wrong length: " + str)

let private pushActors actors stream =
    let a = Map.toList actors
    let nActors = List.length a
    stream @ (string nActors :: List.map exportActor a)

let private popActors (stream: string list) =
    let nActors = int stream.[0]
    (
        List.map importActor stream.[1..nActors]
        |> Map.ofList,
        stream.[(nActors + 1)..]
    )

let exportActorID = string

let importActorID = int

let private pushActorCombatQueue actorCombatQueue stream =
    let nActors = List.length actorCombatQueue
    stream @ (string nActors :: List.map exportActorID actorCombatQueue)

let private popActorCombatQueue (stream: string list) =
    let nActors = int stream.[0]
    (List.map importActorID stream.[1..nActors], stream.[(nActors + 1)..])

let exportActorPosition (id, {X = x; Y = y}) =
    [id; x; y]
    |> List.map string
    |> List.toSeq
    |> String.concat " "

let importActorPosition (str: string) =
    let tokens =
        str.Split " "
        |> Array.toList
        |> List.map int
    tokens.[0], {X = tokens.[1]; Y = tokens.[2]}

let private pushActorCombatPositions actorPositions stream =
    let ap = Map.toList actorPositions
    let nActors = List.length ap
    stream @ (string nActors :: List.map exportActorPosition ap)

let private popActorCombatPositions (stream: string list) =
    let nActors = int stream.[0]
    (
        List.map importActorPosition stream.[1..nActors]
        |> Map.ofList,
        stream.[(nActors + 1)..]
    )

let private exportMapTiles tiles =
    convertMapTilesToString defaultTilesetParser.CombatMapParser tiles

let importMapTiles (tiles: string) =
    tiles
    |> Seq.toList
    |> List.map getMapTile

let private pushMap map stream =
    let tiles =
        map.Tiles
        |> Map.toList
        |> List.sortBy (fun ({X = x; Y = y}, _) -> (y, x))
        |> List.map (fun (_, tile) -> tile)
    stream @ [
        string map.Width
        string map.Height
    ]
    @ List.singleton (exportMapTiles tiles)

let private popMap (stream: string list) =
    let width = int stream.[0]
    let height = int stream.[1]
    let tiles = importMapTiles stream.[2]
    let map = CombatMap.create width height tiles
    (map, stream.[3..])

let private pushRest statusBar (tileset: Tileset) (stream: string list) =
    stream @ [
        string statusBar.Start.X
        string statusBar.Start.Y
        string statusBar.Length
        string tileset
    ]

let exportGameState combatState tileset statusState =
    let {CombatMap = map; Actors = actors; ActorCombatQueue = actorCombatQueue; ActorCombatPositions = actorCombatPositions} = combatState
    let {StatusBar = statusBar} = statusState
    []
    |> pushActors actors
    |> pushActorCombatQueue actorCombatQueue
    |> pushActorCombatPositions actorCombatPositions
    |> pushMap map
    |> pushRest statusBar tileset

let importGameState (stream: string list) =
    let (actors, queueFirst) = popActors stream
    let (actorCombatQueue, positionsFirst) = popActorCombatQueue queueFirst
    let (actorCombatPositions, mapFirst) = popActorCombatPositions positionsFirst
    let (map, rest) = popMap mapFirst
    let combatState = {
        Actors = actors
        ActorCombatQueue = actorCombatQueue
        ActorCombatPositions = actorCombatPositions
        CombatMap = map
    }
    let statusState = {
        StatusBar = {Start = {X = int rest.[0]; Y = int rest.[1]}; Length = int rest.[2]}
        StatusBuffer = {Receiver = Player; Stream = ""}
    }
    let tileset =
        match rest.[3] with
        | "DefaultTileset" -> DefaultTileset
        | "DottedTileset" -> DottedTileset
        | _ -> DefaultTileset
    let action =
        match rest.[3] with
        | "DefaultTileset" -> CompletePlayerAction StartSession
        | "DottedTileset" -> CompletePlayerAction StartSession
        | _ -> CompletePlayerAction StartSessionWithUnknownTileset
    combatState, tileset, statusState, action
