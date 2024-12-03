open System.Text.RegularExpressions

let input = System.IO.File.ReadAllText("day03.txt")

let mults text = 
    printfn $"{text}"
    Regex.Matches(text, @"mul\((\d+)\,(\d+)\)")
    |> List.ofSeq
    |> List.map (fun m ->
        printfn $"{m.Value}"
        int64 m.Groups[1].Value * int64 m.Groups[2].Value)
    |> List.sum

let part1 = mults input

let donts =
    Regex.Matches(input, @"(don't\(\))((?s).*?)(do\(\))")
    |> Seq.map _.Groups[2].Value
    |> Seq.map mults
    |> Seq.sum

let part2 = part1 - donts
