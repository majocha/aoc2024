#r "nuget: FSharpPlus"
open FSharpPlus

open System
let input = System.IO.File.ReadAllLines("day05.txt")

let rules = input |> Seq.takeWhile (String.IsNullOrWhiteSpace >> not) |> Seq.toList |> map (sscanf "%d|%d")
let parseUpdate = String.split [","] >> (Seq.map int) >> toList
let updates = input |> Seq.skip (rules.Length + 1) |> Seq.toList |> map parseUpdate |> toList

let checkRule update (a, b) =
    let ia = update |> List.tryFindIndex ((=) a)
    let ib = update |> List.tryFindIndex ((=) b)
    match ia, ib with 
    | Some i, Some j when i > j -> false
    | _ -> true

let correct = updates |> List.filter (fun u -> rules |> List.forall (checkRule u))

let part1 = correct |> List.map (fun u -> u[(u.Length - 1) / 2]) |> List.sum

let incorrect = updates |> List.except correct

let applyRule update (a, b) =
    let ia = update |> Array.tryFindIndex ((=) a)
    let ib = update |> Array.tryFindIndex ((=) b)

    match ia, ib with 
    | Some i, Some j when i > j ->
        let v = update[j]
        update[j] <- update[i]
        update[i] <- v
        false
    | _ -> true

let ordered update =
    let a = update |> List.toArray
    while not (rules |> List.forall (applyRule a)) do ()
    a[(a.Length - 1) / 2]

let part2 = incorrect |> List.map ordered |> List.sum
