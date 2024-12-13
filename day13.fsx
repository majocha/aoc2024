#r "nuget: FSharpPlus"
#r "nuget: MathNet.Numerics.FSharp, 5.0.0"
open FSharpPlus
open System
open System.IO
open MathNet.Numerics.LinearAlgebra

let systems =
    [
        use input = System.IO.File.OpenText "day13.txt"
        while not input.EndOfStream do
            let ax, ay = input.ReadLine() |> sscanf "Button A: X+%d, Y+%d"
            let bx, by = input.ReadLine() |> sscanf "Button B: X+%d, Y+%d"
            let px, py =input.ReadLine() |> sscanf "Prize: X=%d, Y=%d"

            let A = 
                matrix
                 [[double ax; double bx]
                  [double ay; double by]]

            let b = vector [double px + 10000000000000.0; double py + 10000000000000.0]

            yield A, b
            try input.ReadLine() |> ignore with _ -> ()
    ]


let isInteger (d: double) = Math.Abs(d - Math.Round d) < 0.01

let good (v: double seq) = v |> Seq.forall isInteger

[ for A, b in systems do
    let result = A.Solve b
    if good result then
        result * vector [ 3.0; 1.0 ]
] |> Seq.sum |> printfn "%.3f"


