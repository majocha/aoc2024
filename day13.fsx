#r "nuget: FSharpPlus"
open FSharpPlus
open System
open System.IO

let input = System.IO.File.OpenText "day13.txt"

let configs =
    [
        while not input.EndOfStream do
            let ax, ay = input.ReadLine() |> sscanf "Button A: X+%d, Y+%d"
            let bx, by = input.ReadLine() |> sscanf "Button B: X+%d, Y+%d"
            let px, py =input.ReadLine() |> sscanf "Prize: X=%d, Y=%d"

            yield (ax, bx, px), (ay, by, py)
            try input.ReadLine() |> ignore with _ -> ()
        input.Close()
    ]

let combinations =
    [
        for a in 0 .. 100 do
        for b in 0 .. 100 do
            a, b
    ] |> List.sortBy (fun (a, b) -> 3 * a + b)

let attempt ((ax, bx, px), (ay, by, py)) (na, nb) =
    na * ax + nb * bx = px && na * ay + nb * by = py

let best config =
    combinations |> List.tryFind (attempt config)

let part1 = configs |> List.choose best |> List.sumBy (fun (a, b) -> 3 * a + b)


