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
        | UnknownCommand

    type TextBox = {
        Start: Position;
        Length: int;
    }
