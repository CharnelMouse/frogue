module Status
open Types
open System
open ScreenWriter

let pushStatus statusState text =
    let newStream =
        match statusState.StatusBuffer.Stream with
        | "" -> text
        | a -> a + " " + text
    {statusState with
        StatusBuffer = {statusState.StatusBuffer with Stream = newStream}
    }

let private charsToString = Array.map string >> String.concat ""

let private popStatusLine reset fullLinesOnly statusState =
    let buffer = statusState.StatusBuffer
    let stream = buffer.Stream
    let bar = statusState.StatusBar
    let boxLength = bar.Length
    match fullLinesOnly, stream.Length with
    | _, 0 ->
        statusState, false
    | false, sl when sl <= boxLength ->
        writeBox stream statusState.StatusBar reset
        {statusState with StatusBuffer = {buffer with Stream = ""}}, true
    | true, sl when sl = boxLength ->
        writeBox stream statusState.StatusBar reset
        {statusState with StatusBuffer = {buffer with Stream = ""}}, true
    | true, sl when sl < boxLength ->
        statusState, false
    | _ ->
        let continueString = "<cont.>"
        let stopLength = boxLength - (continueString.Length + 1)
        let poppedString, remainingStream =
            stream.ToCharArray()
            |> Array.splitAt stopLength
            |> (fun (a, b) ->
                (charsToString a).TrimEnd ' ',
                (charsToString b).TrimStart ' '
               )
        writeBox (poppedString + " " + continueString) bar reset
        Console.ReadKey(true) |> ignore
        let remainingBuffer = {buffer with Stream = remainingStream}
        {statusState with StatusBuffer = remainingBuffer}, true

let rec popStatus reset fullLinesOnly statusState =
    let newStatusState, anyPopped = popStatusLine reset fullLinesOnly statusState
    // if remaining stream length = boxLength then last line only partially
    // shows after next pushing non-receiver action shown on map, doesn't
    // seem right but best I can do without looking into actions future
    let remainingStream = newStatusState.StatusBuffer.Stream
    match fullLinesOnly, remainingStream.Length, anyPopped with
    | true, rl, _ when rl < newStatusState.StatusBar.Length ->
        newStatusState
    | _, _, false ->
        newStatusState
    | _, _, true ->
        popStatus reset fullLinesOnly newStatusState

let pushDieMessage statusState =
    pushStatus statusState "You die! Press any key to exit."

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

let pushStatusByController selfStatus otherSuffix object endMark currentActor statusState =
    statusByController selfStatus otherSuffix endMark currentActor object statusState.StatusBuffer.Receiver
    |> pushStatus statusState

let popStatusIfReceiverTurnOrFullLineInBuffer reset currentActor statusState =
    let buffer = statusState.StatusBuffer
    match buffer.Receiver, buffer.Stream.Length with
    | r, _ when r = currentActor.Controller ->
        popStatus reset false statusState
    | _, l when l > statusState.StatusBar.Length ->
        popStatus reset true statusState
    | _ ->
        statusState
