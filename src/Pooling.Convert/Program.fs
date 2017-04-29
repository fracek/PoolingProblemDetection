open Pooling
open Pooling.OSiL
open Pooling.AMPL
open Pooling.ProductQualitiesSolver
open System.IO
open MBrace.FsPickler


let (>>=) a f =
    match a with
    | None -> None
    | Some v -> f v

let writeDot (wr : StreamWriter) (network : Network.T) =
    let writeArc arc =
        match arc with
        | InputOutput (Input (index = i), Output (index = o)) -> wr.WriteLine(sprintf "    I%i -> O%i;" i o)
        | InputPool (Input (index = i), Pool (p, _)) -> wr.WriteLine(sprintf "    I%i -> P%i;" i p)
        | PoolOutput (Pool (p, _), Output (index = o)) -> wr.WriteLine(sprintf "    P%i -> O%i;" p o)
    wr.WriteLine(sprintf "digraph D {")
    wr.WriteLine(sprintf "    rankdir=\"LR\";")

    network.Inputs
    |> List.iter (fun (Input (index = i)) -> wr.WriteLine(sprintf "    I%i [shape=square];" i))
    network.Pools
    |> List.iter (fun (Pool (p, _)) -> wr.WriteLine(sprintf "    P%i [shape=square, style=rounded];" p))
    network.Outputs
    |> List.iter (fun (Output (index = o)) -> wr.WriteLine(sprintf "    O%i [shape=circle];" o))

    network.Arcs
    |> List.iter writeArc

    wr.WriteLine(sprintf "}")


let convertProblem file outfile =
    printfn "Reading %s" file
    let problem = OSiL.Load file

    printfn "Parsing network"

    let convertCommon problem =
        Some problem
        >>= Transform.identifyVariablesFromPathDefinitions
        >>= Transform.identifyInputCapacities
        >>= Transform.identifyOutputCapacities
        >>= Transform.identifyPoolCapacities
        >>= Transform.identifyProductQualities
        >>= Transform.addMissingCapacities
        >>= Transform.replaceBilinearTermsInObjectives

    let convertAdhyaFormulation prob =
        Some prob
        >>= Transform.identifyReductionsBilinear
        >>= Transform.addPathDefinitionsFromReductions
        >>= Transform.replaceBilinearTerms
        >>= Transform.replacePoolOutFlowWithReductions
        >>= convertCommon

    let convertPFormulation problem =
        Some problem
        >>= Transform.identifyMaterialBalances
        >>= Transform.replaceInPoolWithFraction
        >>= Transform.addPathDefinitionsFromMaterialBalances
        >>= Transform.addReductions
        >>= Transform.replaceBilinearTerms
        >>= Transform.replacePoolOutFlowWithReductions
        >>= convertCommon

    let convertAGForm problem =
        Some problem
        >>= Transform.identifyReductionsLinear
        >>= Transform.identifyPathDefinitions
        >>= convertCommon

    let convertAndExtract problem (cf : Problem.T -> Problem.T option) =
        match problem |> cf with
        | None -> problem, None
        | Some p -> match Transform.extractNetwork p with
                    | {PoolingNetwork = None} -> problem, None
                    | {PoolingNetwork = n} -> p, n

    let (normProb, network) =
        match convertAndExtract problem convertPFormulation  with
        | _, None -> match convertAndExtract problem convertAdhyaFormulation with
                     | _, None -> match convertAndExtract problem convertAGForm with
                                  | _, None -> failwith "not converted"
                                  | np, Some n -> np, n
                     | np, Some n -> np, n
        | p, Some n -> p, n


    printfn "Network: %i inputs, %i pools, %i outputs" (List.length network.Inputs) (List.length network.Pools) (List.length network.Outputs)
    printfn "Writing to %s" outfile
    use wr = new StreamWriter(outfile)

    AMPL.WriteDataFile(network, wr)

    use dotWr = new StreamWriter(outfile + ".dot")
    writeDot dotWr network

    ()

type ConvertOption =
    | ConvertFile
    | ConvertDir


[<EntryPoint>]
let main argv =
    if Array.length argv <> 2 then
        printfn "Input and output file/directory needed."
        1
    else
        let input = argv.[0]
        let output = argv.[1]

        let fileattr = File.GetAttributes(input)
        let opt =
            if fileattr.HasFlag(FileAttributes.Directory) then
                ConvertDir
            else
                ConvertFile

        match opt with
        | ConvertFile ->
            let filename = input
            let outfile = output
            convertProblem filename outfile
        | ConvertDir ->
            let dir = new DirectoryInfo(input)
            let inputdir = input
            let outdir = output

            if not <| Directory.Exists(outdir) then
                Directory.CreateDirectory(outdir) |> ignore

            for file in dir.GetFiles() do
                let filename = file.Name
                let outfile = Path.Combine(outdir, filename + ".dat")
                try
                    convertProblem (Path.Combine(input, filename)) outfile
                with
                    | ex -> printfn "Failed %s" ex.Message


        0
