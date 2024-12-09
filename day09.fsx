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


module Part2 =
    let spaces =
        [
            let mutable current = 0
            for i, d in digits |> List.indexed do
                if not (even i) then
                    yield current, d
                current <- current + d
        ]
    
    let files =
        [
            let mutable current = 0
            for i, d in digits |> List.indexed do
                if even i then
                    yield (i / 2), (current, d)
                current <- current + d
        ] |> List.rev

    let rec move spaces result = function
        | [] -> result
        | (fid, (start, length)) as file :: files ->
            match spaces |> List.tryFindIndex (fun (start', length') -> start' < start && length' >= length) with
            | Some i ->
                let start', length' = spaces[i]
                let spaces =
                    if length' = length then
                        spaces |> List.removeAt i
                    else
                        let spaces = spaces |> List.removeAt i
                        let newSpace = start' + length, length' - length
                        newSpace :: spaces
                let result = (fid, (start', length)) :: result
                let spaces = spaces |> List.sortBy fst
                move spaces result files
            | _ ->
                move spaces (file :: result) files

    let checksum files =
        seq {
            for fid, (start, length) in files do
                for i in start .. start + length - 1 do
                    yield int64 (i * fid)
        } |> Seq.sum

    let part2 = move spaces [] files |> checksum
