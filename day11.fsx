let initial =
    [125; 17]
    |> List.map int64

let splitNumber x = 
    let s = string x
    let l = s.Length / 2
    [int64 s[..l-1]; int64 s[l..]]

let rec blinkOne n = function
    | x when n = 0 -> [ x ]
    | 0L -> blinkOne (n - 1) 1L
    | x when (string x |> String.length) % 2 = 0 ->
        splitNumber x |> List.collect (blinkOne (n - 1))
    | x -> blinkOne (n - 1) (x * 2024L)

let blink n counted =
    let folder m (x, c: int64) =
        match m |> Map.tryFind x with
        | None -> m |> Map.add x c
        | Some c1 -> m |> Map.add x (c + c1)

    [
        for x, c in counted do
            for x1 in blinkOne n x do
                x1, c
    ] |> List.fold folder Map.empty |> Map.toList

let rec blink5 n a =
    if n = 0 then a else
        blink 5 a |> blink5 (n - 1)

initial |> List.countBy id |> List.map (fun (x, c) -> x, int64 c) |> blink5 15 |> List.sumBy snd