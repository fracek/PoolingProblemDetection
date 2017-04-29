module internal Pooling.OSiL.Parser

open Pooling
open System
open System.Xml.Linq

let (>>=) m f =
    match m with
    | None -> None
    | Some x -> f x

let osilns = "{os.optimizationservices.org}"

let xname = XName.Get

let elementValue (e : XElement) = Some e.Value

let element name (xel : XElement) =
    match xel.Element(xname (osilns + name)) with
    | null -> None
    | e -> Some e
   
let elements name (xel : XElement) =
    match xel.Elements(xname (osilns + name)) with
    | null -> None
    | e -> Some (List.ofSeq e)

let attribute name (xel : XElement) =
    match xel.Attribute(xname name) with
    | null -> None
    | attr -> Some attr.Value

let parseDouble = function
    | None -> None
    | Some s ->
        match System.Double.TryParse s with
        | (false, _) -> None
        | (_, d) -> Some d

let parseInt = function
    | None -> None
    | Some s ->
        match System.Int32.TryParse s with
        | (false, _) -> None
        | (_, d) -> Some d

let parseBound = function
    | None -> NoBound
    | Some _ as v ->
        match parseDouble v with
        | None -> NoBound
        | Some d -> Bound d

let parseVariable idx (var : XElement) =
    let name =
        match attribute "name" var with
        | None -> ""
        | Some n -> n
    let ub = attribute "ub" var |> parseBound
    let lb = attribute "lb" var |> parseBound
    let variable : Variable =
        {Name = name; Index = idx; UpperBound = ub; LowerBound = lb; Kind = OtherVariable}
    variable

let parseObjectiveLinearCoef (coef : XElement) =
    let index =
        match attribute "idx" coef |> parseInt with
        | None -> failwith "index required for all variables"
        | Some d -> d
    let value =
        match elementValue coef |> parseDouble with
        | None -> failwith "coefficient required for all variables"
        | Some d -> d
    (index, value)

let parseObjective (obj : XElement) (numVars : int) =
    let kind =
        match attribute "maxOrMin" obj with
        | Some "max" -> ObjectiveMax
        | _ -> ObjectiveMin
    let linearCoefficients = Sparse.Vector numVars
    match elements "coef" obj with
    | None -> ()
    | Some coefs ->
        coefs |> List.map parseObjectiveLinearCoef |> List.iter (fun (i, v) -> linearCoefficients.Item i <- v)
    let objective : Objective =
        {Kind = kind;
         QuadraticCoefficients = Sparse.Matrix numVars;
         LinearCoefficients = linearCoefficients}
    objective

let parseConstraint (con : XElement) linearCoef quadraticCoef nonlin =
    let name = 
        match attribute "name" con with
        | None -> ""
        | Some n -> n
    let ub = attribute "ub" con |> parseBound
    let lb = attribute "lb" con |> parseBound
    let constr : Constraint =
        {Name = name;
         UpperBound = ub;
         LowerBound = lb;
         QuadraticCoefficients = quadraticCoef;
         LinearCoefficients = linearCoef;
         NonLinear = nonlin;
         Kind = UnidentifiedConstraint;}
    constr

let expandElements (root : XElement option) =
    let expandElement (el : XElement) =
        let mult = 
            match attribute "mult" el |> parseInt with
            | None -> 1
            | Some d -> d
        let incr =
            match attribute "incr" el |> parseDouble with
            | None -> 0.0
            | Some d -> d
        let value =
            match elementValue el |> parseDouble with
            | None -> failwith "element value required"
            | Some v -> v

        seq { for i in 0..(mult - 1) do yield value + double(i) * incr }

    match root >>= elements "el" with
    | None -> Seq.empty
    | Some els -> Seq.map expandElement els |> Seq.concat

let parseLinearCoefficients (node : XElement) (numVars : int) numConstraints =
    let buildLinearCoefficients terms =
        let lc = Sparse.Vector numVars
        Seq.iter (fun (_, j, v) -> lc.Item j <- v) terms
        lc

    let start = expandElements (element "start" node) |> List.ofSeq
    let columnIndexes = expandElements (element "colIdx" node) |> List.ofSeq |> List.map (fun j -> int j)
    let values = expandElements (element "value" node) |> List.ofSeq
    let rowElements = Seq.map2 (-) (Seq.skip 1 start) start |> List.ofSeq
    let rowIndexes =
        rowElements
        |> List.mapi (fun i count -> List.replicate (int count) i)
        |> List.concat

    let linearCoeffsAtRow =
        Seq.zip3 rowIndexes columnIndexes values
        |> Seq.groupBy (fun (i, _, _) -> i)
        |> Seq.map (fun (i, terms) -> (i, buildLinearCoefficients terms))
        |> Map.ofSeq

    let coefficientsAtIndex i = 
        match Map.tryFind i linearCoeffsAtRow with
        | None -> Sparse.Vector numVars
        | Some v -> v

    match numConstraints with
    | Some n -> seq { for i in 0..(n-1) do yield coefficientsAtIndex i} |> List.ofSeq
    | None -> failwith "numberOfConstraints attribute expected"

let parseQuadraticCoefficients (node : XElement) (numVars : int) =
    let parseQTerm (t : XElement) =
        let fail var exp =
            match exp with
            | None -> failwith ("qTerm does not contain " + var)
            | Some v -> v

        let idx = attribute "idx" t |> parseInt |> fail "idx"
        let idxOne = attribute "idxOne" t |> parseInt |> fail "idxOne"
        let idxTwo = attribute "idxTwo" t |> parseInt |> fail "idxTwo"
        let coef = attribute "coef" t |> parseDouble |> fail "coef"
        (idx, idxOne, idxTwo, coef)

    let convertToMatrix (constraintIdx, coeffs) =
        let m = Sparse.Matrix numVars
        coeffs
        |> Seq.iter (fun (_, idxOne, idxTwo, coef) -> m.Item (idxOne, idxTwo) <- coef)
        (constraintIdx, m)

    match elements "qTerm" node with
    | None -> Map.empty
    | Some terms ->
        terms
        |> List.map parseQTerm
        |> Seq.groupBy (fun (i, _, _, _) -> i)
        |> Seq.map convertToMatrix
        |> Map.ofSeq

let parseNonlinearExpressions (node : XElement) =
    let parseExpression (e : XElement) =
        let fail var exp =
            match exp with
            | None -> failwith ("nl does not contain " + var)
            | Some v -> v
        let idx = attribute "idx" e |> parseInt |> fail "idx"

        let rec parseNode (n : XElement) =
            let (|NodeName|_|) name (n : XElement) =
                if n.Name.ToString() = (osilns + name) then Some n else None

            let child = n.Elements() |> List.ofSeq
            match n with
            | NodeName "nl" n -> (parseNode child.Head)
            | NodeName "negate" n -> Nl.Negate (parseNode child.Head)
            | NodeName "product" n -> Nl.Product (List.map parseNode child)
            | NodeName "times" n -> Nl.Product [(parseNode child.[0]); (parseNode child.[1])]
            | NodeName "sum" n -> Nl.Sum (List.map parseNode child)
            | NodeName "minus" n -> Nl.Minus (List.map parseNode child)
            | NodeName "divide" n -> Nl.Divide (parseNode child.[0], parseNode child.[1])
            | NodeName "exp" n -> Nl.Exp (parseNode child.Head)
            | NodeName "power" n -> Nl.Power ((parseNode child.[0]), (parseNode child.[1]))
            | NodeName "ln" n -> Nl.Ln (parseNode child.Head)
            | NodeName "square" n -> Nl.Power ((parseNode child.Head), Nl.Num 2.0)
            | NodeName "variable" n ->
                match (attribute "idx" n |> parseInt, attribute "coef" n |> parseDouble) with
                | (Some idx, Some c) -> Nl.Var (c, idx)
                | _ -> failwith "failed parsing variable"
            | NodeName "number" n ->
                match attribute "value" n |> parseDouble with
                | Some v -> Nl.Num v
                | _ -> failwith "failed parsing number"
            | _ -> failwith (sprintf "failed parsing node %A" n)
            
        (idx, parseNode e)

    match elements "nl" node with
    | None -> Map.empty
    | Some exps ->
        exps
        |> List.map parseExpression
        |> Map.ofList

let parseOSiL (doc : XDocument) =
    let data = element "instanceData" doc.Root
    let variables =
        match data >>= element "variables" >>= elements "var" with
        | None -> failwith "No variables found"
        | Some vars -> Seq.mapi parseVariable vars
    let numVars = Seq.length variables
    let numConstraints = data >>= element "constraints" >>= attribute "numberOfConstraints" |> parseInt
    let linearCoefficients =
        match data >>= element "linearConstraintCoefficients" with
        | None ->  failwith "No linearConstraintCoefficients found"
        | Some coefs -> parseLinearCoefficients coefs numVars numConstraints
    let quadraticCoefficients =
        match data >>= element "quadraticCoefficients" with
        | None -> failwith "No quadraticCoefficients found"
        | Some coefs -> parseQuadraticCoefficients coefs numVars
    let nonlinearExpressions =
        match data >>= element "nonlinearExpressions" with
        | None -> Map.empty
        | Some exps -> parseNonlinearExpressions exps
    let objectives =
        match data >>= element "objectives" >>= elements "obj" with
        | None -> failwith "No objective found"
        | Some objs -> Seq.map (fun o -> parseObjective o numVars) objs

    // TODO: make this work when multiple objectives
    let objectives =
        match Map.tryFind -1 quadraticCoefficients with
        | None -> objectives
        | Some quadcoef ->
            match objectives |> Seq.toList with
            | [obj] -> [{obj with QuadraticCoefficients = quadcoef}] |> Seq.ofList
            | _ -> objectives

    let quadraticCoefficientsForConstraint i =
        match Map.tryFind i quadraticCoefficients with
        | None -> Sparse.Matrix numVars
        | Some m -> m

    let constraints =
        match data >>= element "constraints" >>= elements "con" with
        | None -> 
            printfn "Warning: no constraints found. Maybe something it's wrong"
            List.empty
        | Some cons -> List.mapi2 (fun i c linCoef ->
            parseConstraint c linCoef (quadraticCoefficientsForConstraint i) (Map.tryFind i nonlinearExpressions)) cons linearCoefficients

    Problem.create (List.ofSeq variables) (List.ofSeq objectives) constraints

