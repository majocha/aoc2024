let input = System.IO.File.ReadAllLines "day16.txt"

let inline (++) (x, y) (dx, dy) = x + dx, y + dy

let dirs =
    [
        1, 0
        0, 1
        -1, 0
        0, -1
    ]

let wrap d = let d = d % 4 in if d < 0 then d + 4 else d
let turn d dx = d + dx |> wrap

let findChar c =
    input |> Seq.indexed |> Seq.map (fun (y, row) ->
        row |> Seq.tryFindIndex ((=) c) |> Option.map (fun x -> x, y))
    |> Seq.choose id |> Seq.head

let start = findChar 'S'
let finish = findChar 'E'

let isWall (x, y) = input[y][x] = '#'

let movesFrom (pos, d, cost) =
    Set [
        pos ++ dirs[d], d, cost + 1L
        pos, (turn d -1), cost + 1000L
        pos, (turn d 1), cost + 1000L
    ]

let maxValue = System.Int64.MaxValue

let rec run costMap moves =
    let folder (costs, next) ((p, d, c) as m) =
        if isWall p || costs |> Map.tryFind (p, d) |> Option.defaultValue maxValue < c then costs, next
        else (costs |> Map.add (p, d) c), (next + movesFrom (p, d, c))
    let costMap, next = moves |> Seq.fold folder (costMap, Set.empty)
    if next |> Set.isEmpty then costMap else run costMap next

let costMap = run Map.empty (Set [ start, 0, 0L ])

let best = costMap |> Map.filter (fun (pos, _) _ -> pos = finish) |> Map.values |> Seq.min

let reverseCostMap = run Map.empty (Set [finish, 1, 0L])

let added = costMap |> Map.map (fun (p, d) c -> reverseCostMap[p, d |> turn 2] + c)
let paths = added |> Map.filter(fun _ c -> c = best) |> Map.keys |> Seq.map fst |> Set.ofSeq |> Seq.length
