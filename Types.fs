namespace Frogue
module Types =
    type Position = {
       X: int;
       Y: int;
    }
    
    type Player = {
        Position: Position;
    }

    type Direction =
        | Up
        | Down
        | Left
        | Right

    type Command =
        | Move of Direction
        | Wait
        | Help
        | Quit
        | Unknown

    type TextBox = {
        Start: Position;
        Length: int;
    }

    type Map = {
        Tiles: string list
        Width: int
        Height: int
    }

    let createMap width height tiles =
        let widthIsValid = List.length(tiles) = height
        let heightIsValid = List.reduce (&&) (List.map (function x -> (String.length(x) = width)) tiles)
        if not (widthIsValid && heightIsValid)
            then failwith "Invalid map"
        {Tiles = tiles; Width = width; Height = height}

    type GameState = {
        Player: Player
        Map: Map
        StatusBar: TextBox
    }
