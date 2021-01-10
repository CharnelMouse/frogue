namespace Frogue
module Main =
    open Types
    open Output
    open SaveSystem
    open Status
    open Action
    open TimeSystem
    open ActionGenerator

    let private levelMap = Map.create 20 10 (importMapTiles [
        "####################"
        "#              +    "
        "#              #   #"
        "#              #   #"
        "#              #   #"
        "#              #   #"
        "#              #   #"
        "#              #   #"
        "#              #   #"
        "####################"
    ])

    let private startingWorldState = {
        Actors = [
            {
                Position = {X = 1; Y = 1}
                Tile = PlayerTile
                Controller = Player
                Name = "adventurer"
                Script = WaitScript
            }
            {
                Position = {X = 7; Y = 6}
                Tile = OrcTile
                Controller = AIController
                Name = "orc"
                Script = DumbHunt
            }
            {
                Position = {X = 8; Y = 5}
                Tile = OrcTile
                Controller = AIController
                Name = "orc"
                Script = DumbHunt
            }
            {
                Position = {X = 17; Y = 7}
                Tile = OrcTile
                Controller = AIController
                Name = "orc"
                Script = DumbHunt
            }
            {
                Position = {X = 16; Y = 8}
                Tile = OrcTile
                Controller = AIController
                Name = "orc"
                Script = DumbHunt
            }
        ]
        Map = levelMap
    }

    let private startingOutputState = {
        StatusBar = {Start = {X = 0; Y = levelMap.Height + 1}; Length = 40}
        StatusBuffer = {Receiver = Player; Stream = ""}
        Tileset = DefaultTileset
    }

    let private startingAction = CompletePlayerAction StartSession

    let rec private mainLoop worldState action outputState =
        let newAction = generateAction worldState action
        let postExecuteWorld = executeAction worldState newAction
        let prePostOutput = updateOutput postExecuteWorld outputState newAction
        let newWorld = updateTime postExecuteWorld newAction
        let anyPlayerActor = List.tryFind (fun a -> a.Controller = Player) newWorld.Actors
        match anyPlayerActor, newAction with
        | None, _ -> prePostOutput |> pushDieMessage |> popStatus false false |> ignore
        | Some _, CompletePlayerAction QuitAction -> popStatus false true prePostOutput |> ignore
        | _ ->
            prePostOutput
            |> popStatusIfReceiverTurnOrFullLineInBuffer true newWorld
            |> mainLoop newWorld newAction

    [<EntryPoint>]
    let private main argv =
        let worldState, outputState, action =
            if saveGameExists "save.sav"
                then loadGame "save.sav"
                else startingWorldState, startingOutputState, startingAction
        updateOutput worldState outputState action
        |> popStatus true true
        |> mainLoop worldState action
        System.Console.ReadLine() |> ignore
        0 // return an integer exit code
