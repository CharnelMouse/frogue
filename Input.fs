namespace Frogue
module Input =
    open System
    open Types
    open Command

    let private getNewCommand() =
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

    let rec private getDirectionOrCancel() =
        let input = Console.ReadKey(true)
        match input.Key with
        | ConsoleKey.LeftArrow  -> Some West
        | ConsoleKey.RightArrow -> Some East
        | ConsoleKey.UpArrow    -> Some North
        | ConsoleKey.DownArrow  -> Some South
        | ConsoleKey.Escape     -> None
        | _                     -> getDirectionOrCancel()

    let private completeDirectionalCommand command =
        match getDirectionOrCancel() with
        | Some direction -> CompleteCommand (command direction)
        | None           -> CompleteCommand Cancel

    let getCommand action =
        match action with
        | CompletePlayerAction _ | CompleteAnyoneAction _ | BlockedAction _ ->
            getNewCommand()
        | IncompleteAction OpenAction -> completeDirectionalCommand OpenTo
        | IncompleteAction CloseAction -> completeDirectionalCommand CloseTo
        | IncompleteAction MindSwapAction -> completeDirectionalCommand MindSwapTo
