#time on
#r "nuget: FSharpPlus"
open FSharpPlus

let xs, ys =
    System.IO.File.ReadAllLines("day01.txt")
    |> List.ofArray
    |> map (sscanf "%d   %d") |> unzip

let part1 = (xs |> sort, ys |> sort) ||> List.zip |> map (fun (x, y) -> abs (x - y)) |> List.sum

let part2 =
    let m = ys |> List.countBy id |> Map
    xs |> List.sumBy (fun x -> x * (m |> Map.tryFind x |> Option.defaultValue 0))