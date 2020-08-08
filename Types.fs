namespace Frogue
module Types =
    type Position = {
       X: int;
       Y: int;
    }
    
    type Player = {
        Position: Position;
    }

    type Command =
        | Left
        | Right
        | Up
        | Down
        | Wait
        | Quit
        | Unknown

    type TextBox = {
        Start: Position;
        Length: int;
    }

    type Map = {
        Tiles: string list
    }

    type GameState = {
        Player: Player;
        Map: Map;
    }
