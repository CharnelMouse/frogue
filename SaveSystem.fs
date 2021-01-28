module SaveSystem
open System.IO
open Types
open Tilesets
open DataConverter
open CombatMap
open DataConverter

let saveGameExists path =
    File.Exists path

let saveGame path worldState outputState =
    File.WriteAllLines (path, exportGameState worldState outputState)

let loadGame path =
    File.ReadAllLines path
    |> Array.toList
    |> importGameState
