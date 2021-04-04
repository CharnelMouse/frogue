module ScreenWriter
open System
open Types

let defaultBackground = ConsoleColor.Black
let defaultForeground = ConsoleColor.White

let initialiseConsole () =
    Console.CursorVisible <- false
    Console.BackgroundColor <- defaultBackground
    Console.ForegroundColor <- defaultForeground

let private cursorTo pos =
    Console.SetCursorPosition(pos.X, pos.Y)

let private resetCursor() =
    cursorTo {X = 0; Y = 0}

let private clearBox box =
    cursorTo box.Start
    String.replicate box.Length " "
    |> Console.Write

let private writeFrom startPos str =
    cursorTo startPos
    Console.Write(str: string)

let writeBox str box reset =
    if String.length(str) > box.Length then failwith "String larger than box length"
    clearBox box
    writeFrom box.Start str
    if reset then resetCursor()

let writeBoxColoured str box reset colour =
    Console.ForegroundColor <- colour
    writeBox str box reset
    Console.ForegroundColor <- defaultForeground

let writeAt pos (symb: char)  =
    cursorTo pos
    Console.Write(symb)
    resetCursor()

let writeAtColoured pos symb colour =
    Console.ForegroundColor <- colour
    writeAt pos symb
    Console.ForegroundColor <- defaultForeground
