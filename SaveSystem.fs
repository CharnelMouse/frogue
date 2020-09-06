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
        | _ -> failwith "invalid actor position"

    let private exportGameState gameState =
        let actorStrings = List.map exportActor gameState.Actors
        let {
            Map = {Width = mW; Height = mH; Tiles = mT}
            StatusBar = {Start = {X = sX; Y = sY}; Length = sL}
            Tileset = tileset
            } = gameState
        string (List.length actorStrings)
        :: actorStrings
        @ [
            string mW
            string mH
            List.map (fun x -> convertInternalTilesToTiles defaultTilesetParser x) mT
            |> List.reduce (fun x y -> x + ";" + y)
            string sX
            string sY
            string sL
            string tileset
        ]

    let private importGameState (strs: string list) =
        let nActors = int strs.[0]
        {
            Actors = List.map importActor strs.[1..nActors]
            Map = createMap (int strs.[nActors + 1]) (int strs.[nActors + 2]) (convertTextTilesToTiles (Array.toList(strs.[nActors + 3].Split ";")))
            StatusBar = {Start = {X = int strs.[nActors + 4]; Y = int strs.[nActors + 5]}; Length = int strs.[nActors + 6]}
            Action =
                match strs.[nActors + 7] with
                | "DefaultTileset" -> CompleteAction StartSession
                | "DottedTileset" -> CompleteAction StartSession
                | _ -> CompleteAction StartSessionWithUnknownTileset
            Tileset =
                match strs.[nActors + 7] with
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
