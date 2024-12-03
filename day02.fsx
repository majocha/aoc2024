#r "nuget:FSharpPLus"
open FSharpPlus

let lines = System.IO.File.ReadAllLines("day02.txt")

let report line = line |> String.split [ " " ] |> Seq.map int |> toList

let rec check' =
    function
    | [] | [ _ ] -> true
    | a :: b :: rest when let d = b - a in d < 1 || d > 3 -> false
    | a :: rest -> check' rest

let rec check canSkip =
    function
    | [] | [ _ ] -> true
    | a :: b :: rest when let d = b - a in d < 1 || d > 3 ->
        if canSkip then check false (a :: rest) else false
    | a :: b :: rest -> check canSkip (b :: rest) || (canSkip && check false (a :: rest))

let bothWays check report = check report || check (report |> List.rev)

let part1 = lines |> Seq.map report |> Seq.filter (bothWays check') |> Seq.length
let part2 = lines |> Seq.map report |> Seq.filter (bothWays (check true)) |> map (tap (printfn "%A")) //|> Seq.length

let withRemoved report =
    [ for i in 0 .. report |> List.length -> report |> List.deleteAt i ]

let part2' = lines |> Seq.map report |> Seq.filter (fun r -> withRemoved r |> Seq.exists (bothWays check')) //|> Seq.length

let suspects = Set part2' -  Set part2 |> toList

let filteredOut = lines |> Seq.map report |> Seq.filter (bothWays (check true) >> not) |> toList

filteredOut |> List.collect withRemoved |> List.find (bothWays check')

check true [9; 1; 4; 5; 8; 11; 12]


for r in suspects do printfn $"%A{r}, {bothWays (check') r}"
for r in suspects do printfn $"%A{r}, {bothWays (check true) r}"
