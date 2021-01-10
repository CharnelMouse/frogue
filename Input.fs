namespace Frogue
module Input =
    open System
    open Types
    open Command

    let rec private getCommandDirection command =
        let input = Console.ReadKey(true)
        match input.Key with
        | ConsoleKey.LeftArrow  -> CompleteCommand (command West)
        | ConsoleKey.RightArrow -> CompleteCommand (command East)
        | ConsoleKey.UpArrow    -> CompleteCommand (command North)
        | ConsoleKey.DownArrow  -> CompleteCommand (command South)
        | ConsoleKey.Escape     -> CompleteCommand Cancel
        | _                     -> getCommandDirection command

    let getCommand action =
        match action with
        | CompletePlayerAction _ | CompleteAnyoneAction _ | BlockedAction _ ->
            let input = Console.ReadKey(true)
            match
              input.KeyChar, input.Key,              int input.Modifiers with
            | 'o'          , _                     , _ -> IncompleteCommand Open
            | 'c'          , _                     , _ -> IncompleteCommand Close
            | 'm'          , _                     , _ -> IncompleteCommand MindSwap
            | '.'          , _                     , _ -> CompleteCommand Wait
            | '?'          , _                     , _ -> CompleteCommand Help
            | 'q'          , _                     , _ -> CompleteCommand Quit
            | 's'          , _                     , _ -> CompleteCommand SaveGameCommand
            | 't'          , _                     , _ -> CompleteCommand ToggleTilesetCommand
            | '\u0000'     , ConsoleKey.LeftArrow  , 0 -> CompleteCommand (Move West)
            | '\u0000'     , ConsoleKey.RightArrow , 0 -> CompleteCommand (Move East)
            | '\u0000'     , ConsoleKey.UpArrow    , 0 -> CompleteCommand (Move North)
            | '\u0000'     , ConsoleKey.DownArrow  , 0 -> CompleteCommand (Move South)
            | _                                        -> CompleteCommand UnknownCommand
        | IncompleteAction OpenAction -> getCommandDirection OpenTo
        | IncompleteAction CloseAction -> getCommandDirection CloseTo
        | IncompleteAction MindSwapAction -> getCommandDirection MindSwapTo
