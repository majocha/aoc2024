#r "nuget: FSharpPlus"
open FSharpPlus
open System.IO
open System

let input = File.ReadAllLines "day15.txt"

let movesList = input |> Seq.skipWhile (String.IsNullOrEmpty >> not) |> Seq.skip 1 |> Seq.concat |> List.ofSeq

let wh = 
    [
        for y, row in input |> Seq.takeWhile (String.IsNullOrEmpty >> not) |> Seq.indexed do
        for x, c in row |> Seq.indexed do
            c, (x, y)
    ]

let move (x, y) = function '<' -> -1, 0 | '>' -> 1, 0 | '^' -> 0, -1 | 'v' -> 0, 1 |  _ -> failwith "move error"

let inline (++) (x, y) (dx, dy) = x + dx, y + dy

module Part1 =
    let rec parseMap robot walls boxes = function
        | ('#', p) :: rest -> parseMap robot (walls |> Set.add p) boxes rest
        | ('@', p) :: rest -> parseMap (Some p) walls boxes rest
        | ('O', p) :: rest -> parseMap robot walls (boxes |> Set.add p) rest
        |  _ :: rest -> parseMap robot walls boxes rest
        | [] -> robot |> Option.get, walls, boxes

    let robot, walls, boxes = parseMap None Set.empty Set.empty wh

    let rec tryPush boxes pusher d =
        if boxes |> Set.contains (pusher ++ d) then tryPush boxes (pusher ++ d) d
        elif walls.Contains (pusher ++ d) then None
        else Some (pusher ++ d)

    let rec doMoves boxes robot = function
        | [] -> boxes
        | c :: rest ->
            let d = move robot c
            if walls.Contains (d ++ robot) then doMoves boxes robot rest
            elif boxes |> Set.contains (d ++ robot) then
                match tryPush boxes robot d with
                | Some p ->
                    let robot = d ++ robot
                    let boxes = boxes |> Set.remove robot |> Set.add p
                    doMoves boxes robot rest
                | _ -> doMoves boxes robot rest
            else doMoves boxes (robot ++ d) rest

    doMoves boxes robot movesList |> Seq.sumBy (fun (x, y) -> x + 100 * y)

module Part2 =
    let rewrite line =
        line |> String.collect (function '#' -> "##" | '@' -> "@." | 'O' -> "[]" | '.' -> ".." | _ -> failwith "error")

    let rec parseMap robot walls boxes = function
    | ('#', p) :: rest -> parseMap robot (walls |> Set.add p) boxes rest
    | ('@', p) :: rest -> parseMap (Some p) walls boxes  rest
    | ('[', p1) :: _ :: rest -> parseMap robot walls (boxes |> Set.add p1) rest
    |  _ :: rest -> parseMap robot walls boxes rest
    | [] -> robot |> Option.get, walls, boxes

    let inline (++) (x, y) (dx, dy) = x + dx, y + dy

    let robot, walls, boxes = parseMap None Set.empty Set.empty wh

    let boxAt p boxes =
        if boxes |> Set.contains p then [p; p ++ (1, 0)]
        elif boxes |> Set.contains (p ++ (-1, 0)) then [(p ++ (-1, 0)); p]
        else []

    let rec tryPush pusher d boxes =
        let pushed = pusher ++ d
        if walls |> Set.contains pushed then None
        else
        match boxAt pushed boxes with
        | [] -> boxes |> Set.remove pushed |> Set.add (pushed ++ d) |> Some
        | [p1; p2] as box ->
            monad {
                let boxes = boxes - Set box
                let! boxes = tryPush p1 d boxes 
                let! boxes = tryPush p2 d boxes
                return (box |> List.map ((++) d) |> Set.ofList) + boxes
            }
        | _ -> failwith "what?"

    let rec doMoves boxes robot = function
    | [] -> boxes
    | c :: rest ->
        let d = move robot c
        if walls.Contains (d ++ robot) then doMoves boxes robot rest
        else
            match tryPush robot d boxes with
            | Some boxes -> doMoves boxes (robot ++ d) rest
            | _ -> doMoves boxes robot rest

    doMoves boxes robot movesList |> toList //|> Seq.sumBy (fun (x, y) -> x + 100 * y)


        