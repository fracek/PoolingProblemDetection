namespace Pooling
open System
open System.IO
open System.Diagnostics
open FSharp.Configuration
open MBrace.FsPickler

module ProductQualitiesSolver =

    type ModFiles = ResXProvider<file="ModFiles.resx">

    type ProductSolver() =
        let writeDifferences (filename : string) differencesU differencesL =
            use wr = new StreamWriter(filename)

            let writeDifferences = Seq.iter (fun (i, j, k, d) -> fprintfn wr "%d %d %d %.2f" i j k d)
            // Print .Data file
            fprintfn wr "data;\n"
            let printUniqueValues vs = vs |> Set.ofSeq |> Set.toList |> List.iter (fprintf wr "%d ")
            fprintf wr "set I := "
            differencesU |> Seq.map (fun (i, _, _, _) -> i) |> printUniqueValues
            fprintfn wr ";"

            fprintf wr "set J := "
            differencesU |> Seq.map (fun (_, j, _, _) -> j) |> printUniqueValues
            fprintfn wr ";"

            fprintf wr "set K := "
            differencesU |> Seq.map (fun (_, _, k, _) -> k) |> printUniqueValues
            fprintfn wr ";"

            fprintfn wr "param DTU :="
            writeDifferences differencesU
            fprintfn wr ";"

            fprintfn wr "param DTL :="
            writeDifferences differencesL
            fprintfn wr ";"

            fprintfn wr "param CON :="
            differencesU |> Seq.map (fun (i, j, _, _) -> i, j) |> Set.ofSeq |> Set.iter (fun (i, j) -> fprintfn wr "%d %d 1.0" i j)
            fprintfn wr ";"

            fprintfn wr "\nend;"

        let parseOutput (outfilename : string) =
            let rd = new StreamReader(outfilename)
            let xmlSer = FsPickler.CreateXmlSerializer()
            let (inSpec, pu, pl) =
                try
                    xmlSer.Deserialize<double list list * double list list * double list list>(rd)
                with
                | :? FsPicklerException -> ([], [], [])

            let outSpec = List.map2 List.zip pl pu
            (inSpec, outSpec)

        member this.Solve(differencesU, differencesL) =
            let tempFileWithExtension ext =
                System.IO.Path.GetTempPath() + System.Guid.NewGuid().ToString() + ext

            let tempDatFileName = tempFileWithExtension ".dat"

            writeDifferences tempDatFileName differencesU differencesL

            let modFileContent =
                match (Seq.isEmpty differencesL, Seq.isEmpty differencesU) with
                    | (true, false) -> ModFiles.FindQualU
                    | (false, true) -> ModFiles.FindQualL
                    | (_, _) -> ModFiles.FindQual

            let modFileName = tempFileWithExtension ".mod"
            File.WriteAllText(modFileName, modFileContent)

            let tempOutFileName = tempFileWithExtension ".out"

            let pro = new Process()
            pro.StartInfo.FileName <- "glpsol"
            pro.StartInfo.UseShellExecute <- false
            pro.StartInfo.RedirectStandardOutput <- true
            pro.StartInfo.Arguments <- sprintf "-m %s -d %s -y %s" modFileName tempDatFileName tempOutFileName

            printfn ".mod = %s" modFileName
            printfn ".dat = %s" tempDatFileName
            pro.Start() |> ignore
            pro.WaitForExit()

            parseOutput tempOutFileName
