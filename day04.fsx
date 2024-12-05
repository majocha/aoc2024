let input = System.IO.File.ReadAllLines("day04.txt")

let Y = input.Length
let X = input[0].Length

let dirs = [ for x in -1 .. 1 do for y in -1 .. 1 do x, y ]
let toTry x y =
    [ for dx, dy in dirs do
        [ for n in 0 .. 3 do x + n * dx, y + n * dy ]
    ]

let stringFrom points =
    points |> Seq.map ( fun (x, y) ->
        input |> Array.tryItem y |> Option.bind (Seq.tryItem x) |> Option.defaultValue ' ')
    |> Array.ofSeq |> System.String

let part1 =
    seq {
        for x in 0 .. X - 1 do
        for y in 0 .. Y - 1 do
        for points in toTry x y do
            stringFrom points
    } |> Seq.filter ((=) "XMAS") |> Seq.length


module Part2 = 
    let input = System.IO.File.ReadAllLines("day04.txt")
    
    let Y = input.Length
    let X = input[0].Length
    
    let dirs = [ for x in [-1; 1] do for y in [-1; 1] do x, y ]
    let toTry x y =
        [ for dx, dy in dirs do
            [ for n in -1 .. 1 do x + n * dx, y + n * dy ]
        ]
    
    let stringFrom points =
        points |> Seq.map ( fun (x, y) ->
            input |> Array.tryItem y |> Option.bind (Seq.tryItem x) |> Option.defaultValue ' ')
        |> Array.ofSeq |> System.String
    
    let part2 =
        seq {
            for x in 0 .. X - 1 do
            for y in 0 .. Y - 1 do
                toTry x y |> List.map stringFrom |> List.filter ((=) "MAS") |> List.length = 2
        } |> Seq.filter id |> Seq.length


