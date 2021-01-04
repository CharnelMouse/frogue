namespace Frogue
module Input =
    open System
    open Types
    open Command

    let rec getCommand action =
        let input = Console.ReadKey(true);
        match action with
        | CompletePlayerAction _ | CompleteAnyoneAction _ | BlockedAction _ ->
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
        | IncompleteAction OpenAction ->
            match input.Key with
            | ConsoleKey.LeftArrow  -> CompleteCommand (OpenTo West)
            | ConsoleKey.RightArrow -> CompleteCommand (OpenTo East)
            | ConsoleKey.UpArrow    -> CompleteCommand (OpenTo North)
            | ConsoleKey.DownArrow  -> CompleteCommand (OpenTo South)
            | ConsoleKey.Escape     -> CompleteCommand (Cancel)
            | _                     -> getCommand action
        | IncompleteAction CloseAction ->
            match input.Key with
            | ConsoleKey.LeftArrow  -> CompleteCommand (CloseTo West)
            | ConsoleKey.RightArrow -> CompleteCommand (CloseTo East)
            | ConsoleKey.UpArrow    -> CompleteCommand (CloseTo North)
            | ConsoleKey.DownArrow  -> CompleteCommand (CloseTo South)
            | ConsoleKey.Escape     -> CompleteCommand (Cancel)
            | _                     -> getCommand action
        | IncompleteAction MindSwapAction ->
            match input.Key with
            | ConsoleKey.LeftArrow  -> CompleteCommand (MindSwapTo West)
            | ConsoleKey.RightArrow -> CompleteCommand (MindSwapTo East)
            | ConsoleKey.UpArrow    -> CompleteCommand (MindSwapTo North)
            | ConsoleKey.DownArrow  -> CompleteCommand (MindSwapTo South)
            | ConsoleKey.Escape     -> CompleteCommand (Cancel)
            | _                     -> getCommand action
