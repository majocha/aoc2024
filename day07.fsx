#r "nuget: FSharpPlus"
open FSharpPlus

let input = System.IO.File.ReadAllLines "day07.txt"

let rec check expected result = function
    | _ when result > expected -> None
    | x :: rest ->
        check expected (result + x) rest
        |> Option.orElse (check expected (result * x) rest)
        |> Option.orElse (check expected (int64 $"{result}{x}") rest)
    | [] when result = expected -> Some result
    | _ -> None

let checkLine line =
    let expected, rest = sscanf "%d: %s" line
    let numbers = rest |> String.split [" "] |> toList |> map int64
    check expected numbers.Head numbers.Tail

let part2 = input |> Seq.choose checkLine |> Seq.sum
