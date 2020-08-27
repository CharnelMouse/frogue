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

    let convertInternalTilesToTiles (parser: TilesetParser) tiles =
        tiles
        |> List.map (parser >> string)
        |> List.toSeq
        |> String.concat "" 

    let private getInternalTileType internalTile =
        match internalTile with
        | ' ' -> EmptyTile
        | '-' -> OpenDoorTile
        | '+' -> ClosedDoorTile
        | '#' -> WallTile
        | '@' -> PlayerTile
        | _ -> UnknownTile

    let convertTextTilesToTiles (textTiles: string list) =
        List.map (function x -> List.map getInternalTileType (Seq.toList x)) textTiles
