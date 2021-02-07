module SaveSystem
open System.IO
open DataConverter

let saveGameExists path =
    File.Exists path

let saveGame path worldState tileset statusState =
    File.WriteAllLines (path, exportGameState worldState tileset statusState)

let loadGame path =
    File.ReadAllLines path
    |> Array.toList
    |> importGameState
