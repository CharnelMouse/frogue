namespace Frogue
module Map =
    open Types

    let posIsOnMap pos map =
        let {X = x; Y = y} = pos
        x >= 0 && x < map.Width && y >= 0 && y < map.Height

    let private getInternalTileType internalTile =
        match internalTile with
        | ' ' -> EmptyTile
        | '-' -> OpenDoorTile
        | '+' -> ClosedDoorTile
        | '#' -> WallTile
        | '@' -> PlayerTile
        | _ -> UnknownTile

    let private convertTextTilesToTiles textTiles =
        List.map (function x -> List.map getInternalTileType (Seq.toList x)) textTiles

    let createMap width height textTiles =
        let widthIsValid = List.length(textTiles) = height
        let heightIsValid = List.reduce (&&) (List.map (function x -> (String.length(x) = width)) textTiles)
        if not (widthIsValid && heightIsValid)
            then failwith "Invalid map"
        {
            Width = width
            Height = height
            TextTiles = textTiles
            Tiles = convertTextTilesToTiles textTiles
        }

    let getTileAt pos map =
        let {X = x; Y = y} = pos
        match posIsOnMap pos map with
        | false -> failwith "position out of map bounds"
        | true -> map.Tiles.[y].[x]
