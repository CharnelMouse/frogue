module DataConverter
open Types
open Tilesets

let private exportActor (id, actor) =
    let {Name = name; ControllerName = controller} = actor
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
    | [id; name; tile; controllerName; script]  ->
        int id,
        {
            Tile = getActorTile (char tile)
            ControllerName = controllerName
            Name = name
            Script =
                match script with
                | "waitAI" -> WaitScript
                | "standgroundAI" -> StandGround
                | "dumbhuntAI" -> DumbHunt
                | _ -> failwith ("invalid actor: unrecognised AI: " + script)
        }
    | _ -> failwith ("invalid actor: wrong length: " + str)

let private exportController (name, controllerType) =
    let ts =
        match controllerType with
        | Player -> "player"
        | AIController -> "ai"
    name + ";" + ts

let private importController (str: string) =
    let tokens = str.Split ';' |> Array.toList
    match tokens with
    | []
    | [_] ->
        failwith "invalid controller: too few parameters"
    | _ :: _ :: _ :: _ ->
        failwith "invalid controller: too many parameters"
    | [n; t] ->
        match t with
        | "player" ->
            n, Player
        | "ai" ->
            n, AIController
        | _ ->
            failwithf "invalid controller: unrecognised type: %s" t

let private pushControllers controllers stream =
    let nControllers = Map.count controllers
    stream @ (string nControllers :: List.map exportController (Map.toList controllers))

let private popControllers (stream: string list) =
    let nControllers = int stream.[0]
    List.map importController stream.[1..nControllers] |> Map.ofList, stream.[(nControllers + 1)..]

let private exportControllerRelation ((c1, c2), r) =
    c1 + ";" + c2 + ";" + string r

let private importControllerRelation (str: string) =
    let tokens = str.Split ';' |> Array.toList
    match tokens with
    | []
    | [_]
    | [_; _] ->
        failwith "invalid controller relation: too few parameters"
    | _ :: _ :: _ :: _ :: _ ->
        failwith "invalid controller relation: too many parameters"
    | [c1; c2; r] ->
        match r with
        | "SameController" ->
            (c1, c2), SameController
        | "Enemy" ->
            (c1, c2), Enemy
        | _ ->
            failwithf "invalid controller relation: unrecognised relation: %s" r

let private pushControllerRelations controllerRelations stream =
    let nControllerRelations = Map.count controllerRelations
    stream @ (string nControllerRelations :: List.map exportControllerRelation (Map.toList controllerRelations))

let private popControllerRelations (stream: string list) =
    let nControllerRelations = int stream.[0]
    List.map importControllerRelation stream.[1..nControllerRelations] |> Map.ofList, stream.[(nControllerRelations + 1)..]

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
    let {
        CombatMap = map
        Actors = actors
        ActorCombatQueue = actorCombatQueue
        ActorCombatPositions = actorCombatPositions
        Controllers = controllers
        ControllerRelations = controllerRelations
        } = combatState
    let {StatusBar = statusBar} = statusState
    []
    |> pushControllers controllers
    |> pushControllerRelations controllerRelations
    |> pushActors actors
    |> pushActorCombatQueue actorCombatQueue
    |> pushActorCombatPositions actorCombatPositions
    |> pushMap map
    |> pushRest statusBar tileset

let importGameState (stream: string list) =
    let (controllers, relationsFirst) = popControllers stream
    let (controllerRelations, actorsFirst) = popControllerRelations relationsFirst
    let (actors, queueFirst) = popActors actorsFirst
    let (actorCombatQueue, positionsFirst) = popActorCombatQueue queueFirst
    let (actorCombatPositions, mapFirst) = popActorCombatPositions positionsFirst
    let (map, rest) = popMap mapFirst
    let combatState = {
        Actors = actors
        ActorCombatQueue = actorCombatQueue
        ActorCombatPositions = actorCombatPositions
        CombatMap = map
        Controllers = controllers
        ControllerRelations = controllerRelations
    }
    let statusState = {
        StatusBar = {Start = {X = int rest.[0]; Y = int rest.[1]}; Length = int rest.[2]}
        StatusBuffer = {Receiver = "player"; Stream = ""}
    }
    let tileset =
        match rest.[3] with
        | "DefaultTileset" -> DefaultTileset
        | "DottedTileset" -> DottedTileset
        | _ -> DefaultTileset
    let startResult =
        match rest.[3] with
        | "DefaultTileset" -> NormalStart
        | "DottedTileset" -> NormalStart
        | _ -> StartWithUnknownTileset
    combatState, tileset, statusState, startResult
