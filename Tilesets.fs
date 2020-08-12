namespace Frogue
module Tilesets =
    open Types
    
    let (defaultTilesetParser: TilesetParser) = fun tile ->
        match tile with
        | EmptyTile -> ' '
        | OpenDoorTile -> '-'
        | ClosedDoorTile -> '+'
        | WallTile -> '#'
        | PlayerTile -> '@'
        | UnknownTile -> failwith "tile not found in tileset"

    let (dottedTilesetParser: TilesetParser) = fun tile ->
        match tile with
        | EmptyTile -> '.'
        | OpenDoorTile -> '-'
        | ClosedDoorTile -> '+'
        | WallTile -> '#'
        | PlayerTile -> '@'
        | UnknownTile -> failwith "tile not found in tileset"
