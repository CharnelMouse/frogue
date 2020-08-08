namespace Frogue
module Screen =
    open System
    open Types

    let cursorTo pos =
        Console.SetCursorPosition(pos.X, pos.Y)

    let resetCursor() =
        cursorTo {X = 0; Y = 0}

    let printMap map =
        Console.Clear()
        for row in map.Tiles do
            Console.WriteLine(row)

    let getTileAt pos map =
        let {X = x; Y = y} = pos
        match (x, y) with
        | (x, y) when
            y < 0
            || y >= List.length(map.Tiles)
            || x < 0
            || x >= map.Tiles.[1].Length
            -> failwith "position out of map bounds"
        | (x, y) -> map.Tiles.[y].[x]

    let writeAt pos symb  =
        cursorTo pos
        Console.Write(symb: char)
        resetCursor()

    let drawTileAt pos map =
        getTileAt pos map
        |> writeAt pos

    let clearBox box =
        cursorTo box.Start
        String.replicate box.Length " "
        |> Console.Write

    let writeFrom startPos str =
        cursorTo startPos
        Console.Write(str: string)

    let writeBox str box reset =
        if String.length(str) > box.Length then failwith "String larger than box length"
        clearBox box
        writeFrom box.Start str
        if reset then resetCursor()
