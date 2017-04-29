module Pooling.AMPL.Data

open System
open System.IO

open Pooling

let private inputName = sprintf "f%d"

let private poolName = sprintf "pl%d"

let private outputName = sprintf "B%d"

let private specName = sprintf "sp%d"

let private capacitycToString cap =
    if Double.IsPositiveInfinity cap then
        "+inf"
    elif Double.IsNegativeInfinity cap then
        "-inf"
    else
        sprintf "%.2f" cap

let private interleaveStrings x xs =
    match xs with
    |[] -> ""
    | xs -> List.reduce (fun a b -> a + x + b) xs

let formatData (network : Network.T) (writer : TextWriter) =
    let wprintf format = Printf.fprintf writer format
    let wprintfn format = Printf.fprintfn writer format

    let specs = seq{1..(Network.numSpecs network)} |> Seq.toList

    wprintfn "data;"
    wprintfn ""

    wprintf "set INPUTS := "
    for Input(index = idx) in network.Inputs do
        wprintf " %s " (inputName idx)
    wprintfn " ;"
    wprintfn ""

    wprintf "set POOLS := "
    for Pool(index = idx) in network.Pools do
        wprintf " %s " (poolName idx)
    wprintfn " ;"
    wprintfn ""

    wprintf "set BLENDS := "
    for Output(index = idx) in network.Outputs do
        wprintf " %s " (outputName idx)
    wprintfn " ;"
    wprintfn ""

    wprintf "set SPECS := "
    for idx in specs do
        wprintf " %s " (specName idx)
    wprintfn " ;"
    wprintfn ""

    writer.Flush()

    wprintfn "param:     capacity     varcost      revenue      :="
    for Input(idx, cap, cost, _) in network.Inputs do
        wprintfn "%-10s %-12s %-12.2f %-12s" (inputName idx) (capacitycToString cap) cost "."

    for Pool(index = idx; capacity = cap) in network.Pools do
        wprintfn "%-10s %-12s %-12s %-12s" (poolName idx) (capacitycToString cap) "." "."

    for Output(idx, cap, revenue, _) in network.Outputs do
        wprintfn "%-10s %-12s %-12s %-12.2f" (outputName idx) (capacitycToString cap) "." revenue
    wprintfn ";"
    wprintfn ""

    writer.Flush()

    wprintf "set INPOOLARCS := "
    network.Arcs
    |> List.choose (fun arc ->
        match arc with
        | InputPool (Input(index = inputIdx), Pool(index = poolIdx)) -> Some (inputIdx, poolIdx)
        | _ -> None)
    |> List.map (fun (inputIdx, poolIdx) -> sprintf "(%s, %s)" (inputName inputIdx) (poolName poolIdx))
    |> interleaveStrings ", "
    |> wprintfn "%s ;"
    wprintfn ""

    wprintf "set OUTPOOLARCS := "
    network.Arcs
    |> List.choose (fun arc ->
        match arc with
        | PoolOutput (Pool(index = poolIdx), Output(index = outIdx)) -> Some (poolIdx, outIdx)
        | _ -> None)
    |> List.map (fun (poolIdx, outIdx) -> sprintf "(%s, %s)" (poolName poolIdx) (outputName outIdx))
    |> interleaveStrings ", "
    |> wprintfn "%s ;"
    wprintfn ""

    wprintf "set INOUTARCS := "
    network.Arcs
    |> List.choose (fun arc ->
        match arc with
        | InputOutput (Input(index = inIdx), Output(index = outIdx)) -> Some (inIdx, outIdx)
        | _ -> None)
    |> List.map (fun (inIdx, outIdx) -> sprintf "(%s, %s)" (inputName inIdx) (outputName outIdx))
    |> interleaveStrings ", "
    |> wprintfn "%s ;"
    wprintfn ""

    writer.Flush()

    wprintfn "param    \t\t speclevel:"
    wprintf  "        "
    for sp in specs do
        wprintf " %-8s " (specName sp)
    wprintfn "  :="
    for Input(index = idx; spec = spec) in network.Inputs do
        wprintf "%-7s " (inputName idx)
        for sp in spec do
            wprintf "%-9.2f " sp
        wprintfn ""
    wprintfn ";"
    wprintfn ""

    writer.Flush()

    wprintfn "param    \t\t minspec:"
    wprintf  "        "
    for sp in specs do
        wprintf " %-8s " (specName sp)
    wprintfn "  :="
    for Output(index = idx; spec = spec) in network.Outputs do
        wprintf "%-7s " (outputName idx)
        for (sp, _) in spec do
            wprintf "%-9.2f " sp
        wprintfn ""
    wprintfn ";"
    wprintfn ""

    writer.Flush()

    wprintfn "param    \t\t maxspec:"
    wprintf  "        "
    for sp in specs do
        wprintf " %-8s " (specName sp)
    wprintfn "  :="
    for Output(index = idx; spec = spec) in network.Outputs do
        wprintf "%-7s " (outputName idx)
        for (_, sp) in spec do
            wprintf "%-9.2f " sp
        wprintfn ""
    wprintfn ";"

    writer.Flush()
    ()
