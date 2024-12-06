let input = System.IO.File.ReadAllLines("day06.txt")

let obstacles, guard =
    let mutable guard = 0, 0
    [
        for y in 0 .. input.Length - 1 do
        for x in 0 .. input[y].Length - 1 do
            if input[y][x] = '^' then guard <- x, y
            if input[y][x] = '#' then
                x, y
    ], guard

let tryPos obstacle (x, y) =
    if (x, y) = obstacle then Some '#'
    else input |> Seq.tryItem y |> Option.bind (Seq.tryItem x)

let dirs = [ 0, -1; 1, 0; 0, 1; -1, 0 ]

let turnRight dir = (dir + 1) % 4

let step (x, y) dir =
    let dx, dy = dirs[dir]
    x + dx, y + dy

let rec walk obstacle visited pos dir =
    if visited |> Set.contains (pos, dir) then true
    else
    let visited = visited |> Set.add (pos, dir)
    let inFront = step pos dir
    match tryPos obstacle inFront with
    | Some '#' -> walk obstacle visited pos (turnRight dir)
    | None -> false
    | _ -> walk obstacle visited inFront dir

let part2 =
    [
        for y in 0 .. input.Length - 1 do
        for x in 0 .. input[y].Length - 1 do
            printfn $"processing {(x, y)}"
            yield walk (x, y) Set.empty guard 0
    ] |> List.countBy id
