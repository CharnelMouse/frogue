module MapWriter
open System
open Types
open Frogue.Map
open Tilesets
open ScreenWriter

let printMap tileset map =
    let tilesetParser =
        match tileset with
        | DefaultTileset -> defaultTilesetParser
        | DottedTileset -> dottedTilesetParser
    Console.Clear()
    for row in map.Tiles do
        row
        |> convertMapTilesToString tilesetParser.MapParser
        |> Console.WriteLine

let getOutputActorTile tileset x =
    match tileset with
    | DefaultTileset -> defaultTilesetParser.ActorParser x
    | DottedTileset -> dottedTilesetParser.ActorParser x

let private getOutputMapTile tileset x =
    match tileset with
    | DefaultTileset -> defaultTilesetParser.MapParser x
    | DottedTileset -> dottedTilesetParser.MapParser x

let printActors tileset actors =
    List.iter (fun x -> writeAt x.Position (getOutputActorTile tileset x.Tile)) actors

let drawTileAt pos map tileset =
    getTileAt pos map
    |> getOutputMapTile tileset
    |> writeAt pos

let changeTileset outputState = 
    let newTileset = 
        match outputState.Tileset with
        | DefaultTileset -> DottedTileset
        | DottedTileset -> DefaultTileset
    {outputState with Tileset = newTileset}
