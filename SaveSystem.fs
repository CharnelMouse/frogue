namespace Frogue
module SaveSystem =
    open System.IO
    open Types
    open Frogue.Map
    open Tilesets

    let private exportActor actor =
        string actor.Position.X + ";"
        + string actor.Position.Y + ";"
        + string (defaultTilesetParser actor.Tile)

    let private importActor (str: string) =
        let vals = str.Split ";"
        match vals.Length with
        | 3 -> {
            Position = {X = int vals.[0]; Y = int vals.[1]}
            Tile = getInternalTileType (char vals.[2])
            }
        | _ -> failwith "invalid actor"

    let private pushActors actors stream =
        let nActors = List.length actors
        stream @ (string nActors :: List.map exportActor actors)

    let private popActors (stream: string list) =
        let nActors = int stream.[0]
        (List.map importActor stream.[1..nActors], stream.[(nActors + 1)..])

    let private exportTiles tiles =
        List.map (convertInternalTilesToTiles defaultTilesetParser) tiles

    let importTiles (tiles: string list) =
        List.map (function x -> List.map getInternalTileType (Seq.toList x))  tiles

    let private pushMap map stream =
        stream @ [
            string map.Width
            string map.Height
        ]
        @ exportTiles map.Tiles

    let private popMap (stream: string list) =
        let width = int stream.[0]
        let height = int stream.[1]
        let tiles = importTiles stream.[2..(height + 1)]
        let map = createMap width height tiles
        (map, stream.[(height + 2)..])

    let private pushRest statusBar tileset (stream: string list) =
        stream @ [
            string statusBar.Start.X
            string statusBar.Start.Y
            string statusBar.Length
            string tileset
        ]

    let private exportGameState gameState =
        let {
            Map = map
            StatusBar = statusBar
            Tileset = tileset
            } = gameState
        pushActors gameState.Actors []
        |> pushMap map
        |> pushRest statusBar tileset

    let private importGameState (stream: string list) =
        let (actors, mapFirst) = popActors stream
        let (map, rest) = popMap mapFirst
        {
            Actors = actors
            Map = map
            StatusBar = {Start = {X = int rest.[0]; Y = int rest.[1]}; Length = int rest.[2]}
            Action =
                match rest.[3] with
                | "DefaultTileset" -> CompleteAction StartSession
                | "DottedTileset" -> CompleteAction StartSession
                | _ -> CompleteAction StartSessionWithUnknownTileset
            Tileset =
                match rest.[3] with
                | "DefaultTileset" -> DefaultTileset
                | "DottedTileset" -> DottedTileset
                | _ -> DefaultTileset
        }

    let saveGameExists path =
        File.Exists path

    let saveGame path gameState =
        File.WriteAllLines (path, exportGameState gameState)

    let loadGame path =
        File.ReadAllLines path
        |> Array.toList
        |> importGameState
