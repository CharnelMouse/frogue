module Status
open Types
open System
open ScreenWriter

let pushStatus text outputState =
    let newStream =
        match outputState.StatusBuffer.Stream with
        | "" -> text
        | a -> a + " " + text
    {outputState with
        StatusBuffer = {outputState.StatusBuffer with Stream = newStream}
    }

let rec popStatus reset fullLinesOnly outputState =
    let buffer = outputState.StatusBuffer
    let stream = buffer.Stream
    let bar = outputState.StatusBar
    let boxLength = bar.Length
    if stream.Length <= boxLength
        then
            writeBox stream outputState.StatusBar reset
            {outputState with StatusBuffer = {buffer with Stream = ""}}
        else
            let space = (char " ")
            let continueString = "<cont.>"
            let stopIndex = boxLength - 2 - continueString.Length
            let stopString = stream.[0..stopIndex]
            let (usedString, usedLength) =
                match stopString.[stopIndex] with
                | a when a = space ->
                    let usedString = stopString.TrimEnd space
                    (usedString, usedString.Length)
                | _ ->
                    let lastSpaceIndex = stopString.LastIndexOf " "
                    let usedString = stopString.[0..lastSpaceIndex].TrimEnd space
                    (usedString, usedString.Length)
            writeBox (usedString + " " + continueString) bar reset
            Console.ReadKey(true) |> ignore
            let remainingBuffer = {buffer with Stream = stream.[usedLength..].TrimStart space}
            // if == boxLength then last line only partially shows
            // after next pushing non-receiver action shown on map,
            // doesn't seem right but best I can do without
            // looking into actions future
            if fullLinesOnly && remainingBuffer.Stream.Length <= boxLength
                then {outputState with StatusBuffer = remainingBuffer}
                else popStatus reset fullLinesOnly {outputState with StatusBuffer = remainingBuffer}

let pushDieMessage outputState =
    pushStatus "You die! Press any key to exit." outputState

let private subjectByController subject receiver =
    match subject.Controller with
    | a when a = receiver -> "You"
    | _ -> "The " + subject.Name

let private statusByController selfStatus otherSuffix endMark actor object receiver =
    let subject = subjectByController actor receiver
    let suffix =
        match actor.Controller with
        | a when a = receiver -> selfStatus
        | _ -> otherSuffix
    subject + " " + suffix +
    match object with
    | None -> endMark
    | Some a when a.Controller = receiver -> " you" + endMark
    | Some a -> " the " + a.Name + endMark

let pushStatusByController selfStatus otherSuffix object endMark currentActor outputState =
    let text = statusByController selfStatus otherSuffix endMark currentActor object outputState.StatusBuffer.Receiver
    pushStatus text outputState

let popStatusIfReceiverTurnOrFullLineInBuffer reset currentActor outputState =
    let buffer = outputState.StatusBuffer
    if buffer.Receiver = currentActor.Controller
        then popStatus reset false outputState
        else
            if buffer.Stream.Length > outputState.StatusBar.Length
                then popStatus reset true outputState
                else outputState
