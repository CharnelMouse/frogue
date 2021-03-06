module Tilesets
open Types

type private CombatActorParser = CombatActorTile -> char

type private CombatMapParser = CombatMapTile -> char

type CombatTilesetParser = {
    CombatActorParser: CombatActorParser
    CombatMapParser: CombatMapParser
}

let (defaultTilesetParser: CombatTilesetParser) = {
    CombatActorParser = fun tile ->
        match tile with
        | PlayerTile -> '@'
        | OrcTile -> 'o'
        | UnknownActorTile -> failwith "actor tile not found in tileset"
    CombatMapParser = fun tile ->
        match tile with
        | EmptyTile -> ' '
        | OpenDoorTile -> '-'
        | ClosedDoorTile -> '+'
        | WallTile -> '#'
        | UnknownMapTile -> failwith "map tile not found in tileset"
}

let (dottedTilesetParser: CombatTilesetParser) = {
    CombatActorParser = fun tile ->
        match tile with
        | PlayerTile -> '@'
        | OrcTile -> 'o'
        | UnknownActorTile -> failwith "actor tile not found in tileset"
    CombatMapParser = fun tile ->
        match tile with
        | EmptyTile -> '.'
        | OpenDoorTile -> '-'
        | ClosedDoorTile -> '+'
        | WallTile -> '#'
        | UnknownMapTile -> failwith "map tile not found in tileset"
}

let convertMapTilesToString (parser: CombatMapParser) (tiles: CombatMapTile list) =
    tiles
    |> List.map (parser >> string)
    |> List.toSeq
    |> String.concat "" 

let getActorTile char =
    match char with
    | '@' -> PlayerTile
    | 'o' -> OrcTile
    | _ -> UnknownActorTile

let getMapTile char =
    match char with
    | ' ' -> EmptyTile
    | '-' -> OpenDoorTile
    | '+' -> ClosedDoorTile
    | '#' -> WallTile
    | _ -> UnknownMapTile
