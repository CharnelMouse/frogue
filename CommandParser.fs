namespace Frogue
module CommandParser =
    open Types
    open Frogue.Map
    open Command
    let private changeAction gameState action = {
        gameState with Action = action
    }

    let parseMoveCommand gameState direction =
        let oldPos = gameState.Actors.Head.Position
        let map = gameState.Map
        let {X = oldX; Y = oldY} = oldPos
        let newPos = neighbour oldPos direction
        let actor = List.tryFind (fun x -> x.Position = newPos) gameState.Actors
        if not (posIsOnMap newPos map)
            then BlockedAction MoveActionBlockedByVoid
        else
            let targetTileType = getTileAt newPos map
            match (actor, targetTileType) with
            | (Some _, _) -> CompleteAnyoneAction (AttackAction newPos)
            | (None, WallTile) -> BlockedAction MoveActionBlockedByWall
            | (None, ClosedDoorTile) -> CompleteAnyoneAction (OpenDoorAction newPos)
            | (None, _) -> CompleteAnyoneAction (MoveAction  (oldPos, newPos))

    let private resolveMoveCommand gameState direction =
        changeAction gameState (parseMoveCommand gameState direction)

    let private resolveOpenToCommand gameState direction =
        let pos = gameState.Actors.Head.Position
        let map = gameState.Map
        let toPos = neighbour pos direction
        let newAction =
            if not (posIsOnMap toPos map)
                then BlockedAction OpenToActionBlockedByVoid
            else
                let targetTileType = getTileAt toPos map
                match targetTileType with
                | ClosedDoorTile -> CompleteAnyoneAction (OpenDoorAction toPos)
                | _ -> BlockedAction OpenToActionBlockedByInvalidTile
        changeAction gameState newAction

    let private resolveCloseToCommand gameState direction =
        let pos = gameState.Actors.Head.Position
        let map = gameState.Map
        let toPos = neighbour pos direction
        let newAction =
            if not (posIsOnMap toPos map)
                then BlockedAction CloseToActionBlockedByVoid
            else
                let targetTileType = getTileAt toPos map
                match targetTileType with
                | OpenDoorTile -> CompleteAnyoneAction (CloseDoorAction toPos)
                | _ -> BlockedAction CloseToActionBlockedByInvalidTile
        changeAction gameState newAction

    let resolveCommand gameState command =
        match command with
        | CompleteCommand (Move direction) -> resolveMoveCommand gameState direction
        | CompleteCommand (OpenTo direction) -> resolveOpenToCommand gameState direction
        | CompleteCommand (CloseTo direction) -> resolveCloseToCommand gameState direction
        | CompleteCommand Wait -> changeAction gameState (CompleteAnyoneAction WaitAction)
        | CompleteCommand Help -> changeAction gameState (CompletePlayerAction HelpAction)
        | CompleteCommand Quit -> changeAction gameState (CompletePlayerAction QuitAction)
        | CompleteCommand Cancel -> changeAction gameState (CompletePlayerAction CancelAction)
        | CompleteCommand SaveGameCommand -> changeAction gameState (CompletePlayerAction SaveGameAction)
        | CompleteCommand ToggleTilesetCommand -> changeAction gameState (CompletePlayerAction ToggleTileSetAction)
        | CompleteCommand UnknownCommand -> changeAction gameState (CompletePlayerAction UnknownAction)
        | IncompleteCommand Open -> changeAction gameState (IncompleteAction OpenAction)
        | IncompleteCommand Close -> changeAction gameState (IncompleteAction CloseAction)
