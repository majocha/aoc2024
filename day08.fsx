let input = System.IO.File.ReadAllLines "day08.txt"

let X = input[0].Length
let Y = input.Length

let antennas =
    [
        for x in 0 .. X - 1 do
        for y in 0 .. Y - 1 do
            match input[x][y] with
            | '.' -> ()
            | c -> c, (x, y)
    ]
    |> List.groupBy fst

let good (x,y) = x >= 0 && y >= 0 && x < X && y < Y

let antinodesOfPair ((x1, y1), (x2, y2)) =
    let dx = x2 - x1
    let dy = y2 - y1
    [
        yield! Seq.initInfinite (fun i -> (x2 + i * dx, y2 + i * dy)) |> Seq.takeWhile good
        yield! Seq.initInfinite (fun i -> (x1 - i * dx, y1 - i * dy)) |> Seq.takeWhile good
    ]

let nodes = 
    [
        for c, ps in antennas do
            let ps = ps |> List.map snd
            for pair in (ps, ps) ||> List.allPairs |> List.filter (fun (p1, p2) -> p1 <> p2) do
                yield! antinodesOfPair pair
    ] |> List.filter good |> Set.ofList

nodes.Count