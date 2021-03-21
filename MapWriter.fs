module MapWriter
open System
open Types
open CombatMap
open Tilesets
open ScreenWriter

let printMap tileset map =
    let tilesetParser =
        match tileset with
        | DefaultTileset -> defaultTilesetParser
        | DottedTileset -> dottedTilesetParser
    Console.Clear()
    for row in List.init map.Height id do
        Map.filter (fun {Y = y} _ -> y = row) map.Tiles
        |> Map.toList
        |> List.map (fun (_, tile) -> tile)
        |> convertMapTilesToString tilesetParser.CombatMapParser
        |> Console.WriteLine

let getOutputActorTile tileset x =
    match tileset with
    | DefaultTileset -> defaultTilesetParser.CombatActorParser x
    | DottedTileset -> dottedTilesetParser.CombatActorParser x

let private getOutputMapTile tileset x =
    match tileset with
    | DefaultTileset -> defaultTilesetParser.CombatMapParser x
    | DottedTileset -> dottedTilesetParser.CombatMapParser x

let printActors tileset actors actorPositions =
    let ids = Map.fold (fun state id _ -> state @ [id]) [] actorPositions
    ids
    |> List.iter (fun id ->
        let pos = Map.find id actorPositions
        let {Tile = tile} = Map.find id actors
        writeAt pos (getOutputActorTile tileset tile)
    )

let drawTileAt pos map tileset =
    tryGetTileAt pos map
    |> Option.bind (getOutputMapTile tileset >> Some)
    |> Option.iter (writeAt pos)

let cycleTileset tileset = 
    match tileset with
    | DefaultTileset -> DottedTileset
    | DottedTileset -> DefaultTileset

let redrawMapScreen tileset combatState =
    printMap tileset combatState.CombatMap
    printActors tileset combatState.Actors combatState.ActorCombatPositions
