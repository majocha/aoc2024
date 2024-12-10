let input = System.IO.File.ReadAllLines "day10.txt" |> Array.map (Seq.map (string >> int) >> Seq.toArray)

let moves (x, y) =
    let h = input[y][x]
    seq {
        for x1, y1 in [x - 1, y; x + 1, y; x, y - 1; x, y + 1] do
            input |> Seq.tryItem y1 |> Option.bind (Seq.tryItem x1)
            |> Option.map (fun v -> (x1, y1), v)
            |> Option.filter (fun r -> snd r = h + 1)
    } |> Seq.choose id

let rec reachableFrom p =
    [
        for p1, v in moves p do
            if v = 9 then p1 else
                yield! reachableFrom p1
    ] |> Set.ofList

let zeros =
    seq {
        for y, xs in input |> Seq.indexed do
        for x, v in xs |> Seq.indexed do
            if v = 0 then (x, y)
    }

zeros |> Seq.map (reachableFrom >> Set.count) |> Seq.sum
