namespace Frogue
module SaveSystem =
    open System.IO
    open Types
    open Frogue.Map
    open Tilesets

    let private exportActor actor =
        let controller =
            match actor.Controller with
            | Player -> "player"
            | AI -> "ai"
        let actorType =
            match actor.Type with
            | Adventurer -> "adventurer"
            | Orc -> "orc"
        let {X = x; Y = y} = actor.Position
        [actorType; string (defaultTilesetParser actor.Tile); controller; string x; string y]
        |> List.toSeq
        |> String.concat ";"

    let private importActor (str: string) =
        let vals = Array.toList (str.Split ";")
        match vals with
        | [actorType; tile; controller; x; y]  ->
            {
                Position = {X = int x; Y = int y}
                Tile = getInternalTileType (char tile)
                Controller =
                    match controller with
                    | "player" -> Player
                    | "ai" -> AI
                    | _ -> failwith ("invalid actor: unrecognised controller: " + controller)
                Type =
                    match actorType with
                    | "adventurer" -> Adventurer
                    | "orc" -> Orc
                    | _ -> failwith ("invalid actor: unrecognised type: " + actorType)
            }
        | _ -> failwith ("invalid actor: wrong length: " + str)

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
            StatusBuffer = ""
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
