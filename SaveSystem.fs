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

    let private pushMap map stream =
        stream @ [
            string map.Width
            string map.Height
            List.map (convertInternalTilesToTiles defaultTilesetParser) map.Tiles
            |> List.reduce (fun x y -> x + ";" + y)
        ]

    let private popMap (stream: string list) =
        let width = int stream.[0]
        let height = int stream.[1]
        let tiles = convertTextTilesToTiles (Array.toList(stream.[2].Split ";"))
        let map = createMap width height tiles
        (map, stream.[3..])

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

    let saveGameExists () =
        File.Exists "save.sav"

    let saveGame gameState =
        File.WriteAllLines ("save.sav", exportGameState gameState)

    let loadGame () =
        File.ReadAllLines "save.sav"
        |> Array.toList
        |> importGameState
