namespace Frogue
module Tilesets =
    open Types
    
    type private ActorParser = ActorTile -> char
    type private MapParser = MapTile -> char

    type TilesetParser = {
        ActorParser: ActorParser
        MapParser: MapParser
    }

    let (defaultTilesetParser: TilesetParser) = {
        ActorParser = fun tile ->
            match tile with
            | PlayerTile -> '@'
            | OrcTile -> 'o'
            | UnknownActorTile -> failwith "actor tile not found in tileset"
        MapParser = fun tile ->
            match tile with
            | EmptyTile -> ' '
            | OpenDoorTile -> '-'
            | ClosedDoorTile -> '+'
            | WallTile -> '#'
            | UnknownMapTile -> failwith "map tile not found in tileset"
    }

    let (dottedTilesetParser: TilesetParser) = {
        ActorParser = fun tile ->
            match tile with
            | PlayerTile -> '@'
            | OrcTile -> 'o'
            | UnknownActorTile -> failwith "actor tile not found in tileset"
        MapParser = fun tile ->
            match tile with
            | EmptyTile -> '.'
            | OpenDoorTile -> '-'
            | ClosedDoorTile -> '+'
            | WallTile -> '#'
            | UnknownMapTile -> failwith "map tile not found in tileset"
    }

    let convertMapTilesToString (parser: MapParser) (tiles: MapTile list) =
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
