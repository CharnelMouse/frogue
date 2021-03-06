module FileActor
open Types
open SaveSystem

type SaveGameMessage = {
    CombatState: CombatState
    Tileset: Tileset
    StatusState: StatusState
}

type FileMessage =
| SaveGameMessage of SaveGameMessage
| LoadGameRequest of AsyncReplyChannel<(CombatState * Tileset * StatusState * Action) option>

type FileActor = MailboxProcessor<FileMessage>

let fileAgentBody path (inbox: MailboxProcessor<FileMessage>) =
    let rec loop () = async {
        let! msg = inbox.Receive()
        match msg with
        | SaveGameMessage {CombatState = combatState; Tileset = tileset; StatusState = statusState} ->
            saveGame path combatState tileset statusState
        | LoadGameRequest replyChannel ->
            if saveGameExists path then
                replyChannel.Reply (Some (loadGame path))
            else
                replyChannel.Reply None
        return! loop ()
    }
    loop ()

let startFileAgent = fileAgentBody >> MailboxProcessor.Start
