module SaveSystem
open System.IO
open DataConverter

let saveGameExists path =
    File.Exists path

let saveGame path combatState tileset statusState =
    File.WriteAllLines (path, exportGameState combatState tileset statusState)

let loadGame path =
    File.ReadAllLines path
    |> Array.toList
    |> importGameState
