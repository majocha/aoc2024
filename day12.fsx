let input = System.IO.File.ReadAllLines "day12.txt"

let tryCoords (x, y) =
    input
    |> Array.tryItem y
    |> Option.bind (Seq.tryItem x)

let rec measure c boundary visited perimeter =
    let visited  = visited |> Set.union boundary
    let belongs u = u |> tryCoords |> Option.exists ((=) c)
    let isPerimeter u = not (visited |> Set.contains u) && not (belongs u)

    let next = 
        [
            for (x, y) in boundary do
                yield! [ x - 1, y; x + 1, y; x, y - 1; x, y + 1 ]
        ]

    let perimeter = perimeter + (next |> Seq.filter isPerimeter |> Seq.length)
    let boundary = Set next - visited |> Set.filter belongs

    if boundary.IsEmpty then visited, perimeter else measure c boundary visited perimeter

let all = Seq.init input.Length id |> Seq.collect (fun y -> Seq.init input[0].Length id |> Seq.map (fun x -> (x, y))) |> Set.ofSeq

let rec measureAll remaining result =
    if remaining |> Set.isEmpty then result else
        let (x, y) as u = remaining |> Set.minElement
        let c = input[y][x]
        let v, p  = measure c (Set.singleton u) Set.empty 0
        let a = v.Count
        printfn $" {a} * {p} = {a * p}"
        measureAll (remaining - v) (result + a * p)

let part1 = measureAll all 0
