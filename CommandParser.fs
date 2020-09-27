namespace Frogue
module CommandParser =
    open Types
    open Frogue.Map
    open Command
    let private changeAction worldState action = {
        worldState with Action = action
    }

    let parseMoveCommand worldState direction =
        let oldPos = worldState.Actors.Head.Position
        let map = worldState.Map
        let newPos = neighbour oldPos direction
        let actor = List.tryFind (fun x -> x.Position = newPos) worldState.Actors
        let controller =
            match actor with
                | Some act -> Some act.Controller
                | None -> None
        if not (posIsOnMap newPos map)
            then BlockedAction MoveActionBlockedByVoid
        else
            let targetTileType = getTileAt newPos map
            match (controller, targetTileType) with
            | (Some cont, _) when cont = worldState.Actors.Head.Controller -> BlockedAction MoveActionBlockedByAlly
            | (Some _, _) -> CompleteAnyoneAction (AttackAction newPos)
            | (None, WallTile) -> BlockedAction MoveActionBlockedByWall
            | (None, ClosedDoorTile) -> CompleteAnyoneAction (OpenDoorAction newPos)
            | (None, _) -> CompleteAnyoneAction (MoveAction  (oldPos, newPos))

    let private resolveMoveCommand worldState direction =
        changeAction worldState (parseMoveCommand worldState direction)

    let private resolveOpenToCommand worldState direction =
        let pos = worldState.Actors.Head.Position
        let map = worldState.Map
        let toPos = neighbour pos direction
        let newAction =
            if not (posIsOnMap toPos map)
                then BlockedAction OpenToActionBlockedByVoid
            else
                let targetTileType = getTileAt toPos map
                match targetTileType with
                | ClosedDoorTile -> CompleteAnyoneAction (OpenDoorAction toPos)
                | _ -> BlockedAction OpenToActionBlockedByInvalidTile
        changeAction worldState newAction

    let private resolveCloseToCommand worldState direction =
        let pos = worldState.Actors.Head.Position
        let map = worldState.Map
        let toPos = neighbour pos direction
        let newAction =
            if not (posIsOnMap toPos map)
                then BlockedAction CloseToActionBlockedByVoid
            else
                let targetTileType = getTileAt toPos map
                match targetTileType with
                | OpenDoorTile -> CompleteAnyoneAction (CloseDoorAction toPos)
                | _ -> BlockedAction CloseToActionBlockedByInvalidTile
        changeAction worldState newAction

    let resolveCommand worldState command =
        match command with
        | CompleteCommand (Move direction) -> resolveMoveCommand worldState direction
        | CompleteCommand (OpenTo direction) -> resolveOpenToCommand worldState direction
        | CompleteCommand (CloseTo direction) -> resolveCloseToCommand worldState direction
        | CompleteCommand Wait -> changeAction worldState (CompleteAnyoneAction WaitAction)
        | CompleteCommand Help -> changeAction worldState (CompletePlayerAction HelpAction)
        | CompleteCommand Quit -> changeAction worldState (CompletePlayerAction QuitAction)
        | CompleteCommand Cancel -> changeAction worldState (CompletePlayerAction CancelAction)
        | CompleteCommand SaveGameCommand -> changeAction worldState (CompletePlayerAction SaveGameAction)
        | CompleteCommand ToggleTilesetCommand -> changeAction worldState (CompletePlayerAction ToggleTileSetAction)
        | CompleteCommand UnknownCommand -> changeAction worldState (CompletePlayerAction UnknownAction)
        | IncompleteCommand Open -> changeAction worldState (IncompleteAction OpenAction)
        | IncompleteCommand Close -> changeAction worldState (IncompleteAction CloseAction)
