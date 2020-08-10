namespace Frogue
module SaveSystem =
    open System.IO
    open Types
    open Tilesets

    let convertGameStateToText gameState =
        let {
            Player = {Position = {X = x; Y = y}}
            Map = {Width = mW; Height = mH; Tiles = mT}
            StatusBar = {Start = {X = sX; Y = sY}; Length = sL}
            LastAction = _
            } = gameState
        [
            string x
            string y
            string mW
            string mH
            List.reduce (fun x y -> x + ";" + y) mT
            string sX
            string sY
            string sL
        ]

    let convertTextToGameState (strs: string list) =
        {
            Player = {Position = {X = int strs.[0]; Y = int strs.[1]}}
            Map = {Width = int strs.[2]; Height = int strs.[3]; Tiles = Array.toList(strs.[4].Split ";")}
            StatusBar = {Start = {X = int strs.[5]; Y = int strs.[6]}; Length = int strs.[7]}
            LastAction = CompleteAction StartSession
            Tileset = DefaultTileset
        }

    let saveGameExists () =
        File.Exists "save.sav"

    let saveGame gameState =
        File.WriteAllLines ("save.sav", convertGameStateToText gameState)

    let loadGame () =
        File.ReadAllLines "save.sav"
        |> Array.toList
        |> convertTextToGameState
