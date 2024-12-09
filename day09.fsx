let input = System.IO.File.ReadAllText "day09.txt"

let digits = input |> Seq.map (string >> int) |> Seq.toList

let even x = x % 2 = 0

let spaces =
    [
        let mutable current = 0
        for i, d in digits |> List.indexed do
            if not (even i) then
                for p in current .. current + d - 1 do
                    yield p
            current <- current + d
    ]

let blocks =
    [
        let mutable current = 0
        for i, d in digits |> List.indexed do
            if even i then
                for p in current .. current + d - 1 do
                    yield p, (i / 2)
            current <- current + d
    ] |> List.rev

let rec checksum result i = function
    | [] -> result
    | (block, fid) :: blocks when block <= spaces[i] ->
        let result = result + int64 (block * fid)
        checksum result i blocks
    | (block, fid) :: blocks ->
        let result = result + int64 (spaces[i] * fid)
        checksum result (i + 1) blocks

let part1 = checksum 0L 0 blocks