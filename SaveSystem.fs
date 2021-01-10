module SaveSystem
open System.IO
open Types
open Tilesets
open Frogue

let private exportActor actor =
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
    let {X = x; Y = y} = actor.Position
    [name; string (defaultTilesetParser.ActorParser actor.Tile); controller; script; string x; string y]
    |> List.toSeq
    |> String.concat ";"

let private importActor (str: string) =
    let vals = Array.toList (str.Split ";")
    match vals with
    | [name; tile; controller; script; x; y]  ->
        {
            Position = {X = int x; Y = int y}
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
    let nActors = List.length actors
    stream @ (string nActors :: List.map exportActor actors)

let private popActors (stream: string list) =
    let nActors = int stream.[0]
    (List.map importActor stream.[1..nActors], stream.[(nActors + 1)..])

let private exportMapTiles tiles =
    List.map (convertMapTilesToString defaultTilesetParser.MapParser) tiles

let importMapTiles (tiles: string list) =
    List.map (function x -> List.map getMapTile (Seq.toList x))  tiles

let private pushMap map stream =
    stream @ [
        string map.Width
        string map.Height
    ]
    @ exportMapTiles map.Tiles

let private popMap (stream: string list) =
    let width = int stream.[0]
    let height = int stream.[1]
    let tiles = importMapTiles stream.[2..(height + 1)]
    let map = Map.create width height tiles
    (map, stream.[(height + 2)..])

let private pushRest statusBar tileset (stream: string list) =
    stream @ [
        string statusBar.Start.X
        string statusBar.Start.Y
        string statusBar.Length
        string tileset
    ]

let private exportGameState worldState outputState =
    let {Map = map; Actors = actors} = worldState
    let {StatusBar = statusBar; Tileset = tileset} = outputState
    pushActors actors []
    |> pushMap map
    |> pushRest statusBar tileset

let private importGameState (stream: string list) =
    let (actors, mapFirst) = popActors stream
    let (map, rest) = popMap mapFirst
    let worldState = {
        Actors = actors
        Map = map
    }
    let outputState = {
        StatusBar = {Start = {X = int rest.[0]; Y = int rest.[1]}; Length = int rest.[2]}
        StatusBuffer = {Receiver = Player; Stream = ""}
        Tileset =
            match rest.[3] with
            | "DefaultTileset" -> DefaultTileset
            | "DottedTileset" -> DottedTileset
            | _ -> DefaultTileset
    }
    let action =
        match rest.[3] with
        | "DefaultTileset" -> CompletePlayerAction StartSession
        | "DottedTileset" -> CompletePlayerAction StartSession
        | _ -> CompletePlayerAction StartSessionWithUnknownTileset
    worldState, outputState, action

let saveGameExists path =
    File.Exists path

let saveGame path worldState outputState =
    File.WriteAllLines (path, exportGameState worldState outputState)

let loadGame path =
    File.ReadAllLines path
    |> Array.toList
    |> importGameState
