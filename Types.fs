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
        | North
        | South
        | East
        | West

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
