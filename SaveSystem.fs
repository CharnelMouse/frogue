namespace Frogue
module SaveSystem =
    open System.IO
    open Types
    open Frogue.Map
    open Tilesets

    let private convertGameStateToText gameState =
        let {
            Player = {Position = {X = x; Y = y}}
            Map = {Width = mW; Height = mH; Tiles = mT}
            StatusBar = {Start = {X = sX; Y = sY}; Length = sL}
            Action = _
            Tileset = tileset
            } = gameState
        [
            string x
            string y
            string mW
            string mH
            List.map (fun x -> convertInternalTilesToTiles defaultTilesetParser x) mT
            |> List.reduce (fun x y -> x + ";" + y)
            string sX
            string sY
            string sL
            string tileset
        ]

    let private convertTextToGameState (strs: string list) =
        {
            Player = {Position = {X = int strs.[0]; Y = int strs.[1]}}
            Map = createMap (int strs.[2]) (int strs.[3]) (convertTextTilesToTiles (Array.toList(strs.[4].Split ";")))
            StatusBar = {Start = {X = int strs.[5]; Y = int strs.[6]}; Length = int strs.[7]}
            Action =
                match strs.[8] with
                | "DefaultTileset" -> CompleteAction StartSession
                | "DottedTileset" -> CompleteAction StartSession
                | _ -> CompleteAction StartSessionWithUnknownTileset
            Tileset =
                match strs.[8] with
                | "DefaultTileset" -> DefaultTileset
                | "DottedTileset" -> DottedTileset
                | _ -> DefaultTileset
        }

    let saveGameExists () =
        File.Exists "save.sav"

    let saveGame gameState =
        File.WriteAllLines ("save.sav", convertGameStateToText gameState)

    let loadGame () =
        File.ReadAllLines "save.sav"
        |> Array.toList
        |> convertTextToGameState
