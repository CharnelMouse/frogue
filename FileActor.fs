module FileActor
open Types
open SaveSystem

type SaveGameMessage = {
    WorldState: WorldState
    Tileset: Tileset
    StatusState: StatusState
}

type FileMessage =
| SaveGameMessage of SaveGameMessage
| LoadGameRequest of AsyncReplyChannel<(WorldState * Tileset * StatusState * Action) option>

type FileActor = MailboxProcessor<FileMessage>

let fileAgentBody path (inbox: MailboxProcessor<FileMessage>) =
    let rec loop () = async {
        let! msg = inbox.Receive()
        match msg with
        | SaveGameMessage {WorldState = worldState; Tileset = tileset; StatusState = statusState} ->
            saveGame path worldState tileset statusState
        | LoadGameRequest replyChannel ->
            if saveGameExists path then
                replyChannel.Reply (Some (loadGame path))
            else
                replyChannel.Reply None
        return! loop ()
    }
    loop ()

let startFileAgent = fileAgentBody >> MailboxProcessor.Start
