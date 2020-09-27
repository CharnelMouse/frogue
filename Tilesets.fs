namespace Frogue
module Tilesets =
    open Types
    
    type private TilesetParser = InternalTile -> char

    let (defaultTilesetParser: TilesetParser) = fun tile ->
        match tile with
        | EmptyTile -> ' '
        | OpenDoorTile -> '-'
        | ClosedDoorTile -> '+'
        | WallTile -> '#'
        | PlayerTile -> '@'
        | OrcTile -> 'o'
        | UnknownTile -> failwith "tile not found in tileset"

    let (dottedTilesetParser: TilesetParser) = fun tile ->
        match tile with
        | EmptyTile -> '.'
        | OpenDoorTile -> '-'
        | ClosedDoorTile -> '+'
        | WallTile -> '#'
        | PlayerTile -> '@'
        | OrcTile -> 'o'
        | UnknownTile -> failwith "tile not found in tileset"

    let convertInternalTilesToTiles (parser: TilesetParser) tiles =
        tiles
        |> List.map (parser >> string)
        |> List.toSeq
        |> String.concat "" 

    let getInternalTileType internalTile =
        match internalTile with
        | ' ' -> EmptyTile
        | '-' -> OpenDoorTile
        | '+' -> ClosedDoorTile
        | '#' -> WallTile
        | '@' -> PlayerTile
        | 'o' -> OrcTile
        | _ -> UnknownTile
