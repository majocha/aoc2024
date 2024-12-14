#r "nuget: FSharpPlus"
open FSharpPlus

let input =
    [
        for px, py, vx, vy in System.IO.File.ReadAllLines("day14.txt") |> Seq.map (sscanf "p=%d,%d v=%d,%d") do
            (px, py), (vx, vy)
    ]

let X, Y = 101, 103

let wrap n p =
    let r = p % n
    if r < 0 then r + n else r

let pos t ((px, py), (vx, vy)) =
   wrap X (px + t * vx), wrap Y (py + t * vy)

let after t = input |> List.map (pos t)

let res = after 100
for y in 0 .. Y do
    for x in 0 .. X do
        let c = res |> List.filter ((=) (x,y)) |> List.length
        if c = 0 then printf "." else printf $"{c}"
    printfn ""

let half X x =
    match sign (X / 2 - x) with
    | 0 -> None
    | d -> Some d

let quadrant (x, y) =
    monad {
        let! x = half X x
        let! y = half Y y 
        return x, y
    }

let part1 = 
    after 100
    |> List.map quadrant
    |> List.choose id |>
    List.countBy id
    |> List.map snd
    |> List.reduce ((*))
