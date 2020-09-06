namespace Frogue
module CommandParser =
    open Types
    open Frogue.Map
    let private changeAction gameState action = {
        gameState with Action = action
    }

    let private resolveMoveCommand gameState direction =
        let oldPos = gameState.Actors.Head.Position
        let map = gameState.Map
        let {X = oldX; Y = oldY} = oldPos
        let newPos =
            match direction with
            | North -> {X = oldX; Y = oldY - 1}
            | South -> {X = oldX; Y = oldY + 1}
            | West -> {X = oldX - 1; Y = oldY}
            | East -> {X = oldX + 1; Y = oldY}
        let actor = List.tryFind (fun x -> x.Position = newPos) gameState.Actors
        let newAction =
            if not (posIsOnMap newPos map)
                then BlockedAction MoveActionBlockedByVoid
            else
                let targetTileType = getTileAt newPos map
                match (actor, targetTileType) with
                | (Some _, _) -> BlockedAction MoveActionBlockedByActor
                | (None, WallTile) -> BlockedAction MoveActionBlockedByWall
                | (None, ClosedDoorTile) -> CompleteAction (OpenDoorAction newPos)
                | (None, _) -> CompleteAction (MoveAction  (oldPos, newPos))
        changeAction gameState newAction

    let private resolveOpenToCommand gameState direction =
        let x = gameState.Actors.Head.Position.X
        let y = gameState.Actors.Head.Position.Y
        let map = gameState.Map
        let toPos =
            match direction with
            | North -> {X = x; Y = y - 1}
            | South -> {X = x; Y = y + 1}
            | West -> {X = x - 1; Y = y}
            | East -> {X = x + 1; Y = y}
        let newAction =
            if not (posIsOnMap toPos map)
                then BlockedAction OpenToActionBlockedByVoid
            else
                let targetTileType = getTileAt toPos map
                match targetTileType with
                | ClosedDoorTile -> CompleteAction (OpenDoorAction toPos)
                | _ -> BlockedAction OpenToActionBlockedByInvalidTile
        changeAction gameState newAction

    let private resolveCloseToCommand gameState direction =
        let pos = gameState.Actors.Head.Position
        let map = gameState.Map
        let toPos =
            match direction with
            | North -> {X = pos.X; Y = pos.Y - 1}
            | South -> {X = pos.X; Y = pos.Y + 1}
            | West -> {X = pos.X - 1; Y = pos.Y}
            | East -> {X = pos.X + 1; Y = pos.Y}
        let newAction =
            if not (posIsOnMap toPos map)
                then BlockedAction CloseToActionBlockedByVoid
            else
                let targetTileType = getTileAt toPos map
                match targetTileType with
                | OpenDoorTile -> CompleteAction (CloseDoorAction toPos)
                | _ -> BlockedAction CloseToActionBlockedByInvalidTile
        changeAction gameState newAction

    let resolveCommand gameState command =
        match command with
        | CompleteCommand (Move direction) -> resolveMoveCommand gameState direction
        | CompleteCommand (OpenTo direction) -> resolveOpenToCommand gameState direction
        | CompleteCommand (CloseTo direction) -> resolveCloseToCommand gameState direction
        | CompleteCommand Wait -> changeAction gameState (CompleteAction WaitAction)
        | CompleteCommand Help -> changeAction gameState (CompleteAction HelpAction)
        | CompleteCommand Quit -> changeAction gameState (CompleteAction QuitAction)
        | CompleteCommand Cancel -> changeAction gameState (CompleteAction CancelAction)
        | CompleteCommand SaveGameCommand -> changeAction gameState (CompleteAction SaveGameAction)
        | CompleteCommand ToggleTilesetCommand -> changeAction gameState (CompleteAction ToggleTileSetAction)
        | CompleteCommand UnknownCommand -> changeAction gameState (CompleteAction UnknownAction)
        | IncompleteCommand Open -> changeAction gameState (IncompleteAction OpenAction)
        | IncompleteCommand Close -> changeAction gameState (IncompleteAction CloseAction)
