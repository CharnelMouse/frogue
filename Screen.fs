namespace Frogue
module Screen =
    open System
    open Types
    let printMap map =
        Console.Clear()
        for row in map.Tiles do
            Console.Write(row + "\n")

    let writeAt pos symb  =
        Console.SetCursorPosition(pos.X, pos.Y)
        Console.Write(symb: char)

    let clearBox box =
        Console.SetCursorPosition(box.Start.X, box.Start.Y)
        String.replicate box.Length " "
        |> Console.Write

    let writeFrom from str =
        Console.SetCursorPosition(from.X, from.Y)
        Console.Write(str: string)

    let writeBox str box reset =
        if String.length(str) > box.Length then failwith "String larger than box length"
        clearBox box
        writeFrom box.Start str
        if reset then Console.SetCursorPosition(0, 0)
