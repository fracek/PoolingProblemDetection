module Pooling.Transform


open Pooling.Problem
open Pooling.ProductQualitiesSolver

let private linearCoefficients (con : Constraint) = con.LinearCoefficients.NonZeroValues
let private quadraticCoefficients (con : Constraint) = con.QuadraticCoefficients.NonZeroValues

module internal Matching =

    // Reduction Constraints in form sum(i, v(i,l,j)) - y(l,j) = 0
    let isReductionLinear (con : Constraint) =
        let linValues = linearCoefficients con
        let poolToOutFlow =
            linValues
            |> List.filter (fun (_, v) -> v = -1.0)
        let pathFlow =
            linValues
            |> List.filter (fun (_, v) -> v = 1.0)
        // only these 2 type of coefficients exist
        (pathFlow.Length + 1) = linValues.Length &&
        poolToOutFlow.Length = 1 &&
        (List.length (quadraticCoefficients con) = 0)

    let (|ReductionLinear|_|) (con : Constraint) =
        if isReductionLinear con then Some con
        else None

    // Reduction Constraints in form sum(i, q(i,l) * y(l,j)) - y(l,j) = 0
    let isReductionBilinear (con : Constraint) =
        let linValues = linearCoefficients con
        let quadValues = quadraticCoefficients con
        let poolToOutFlow =
            linValues
            |> List.filter (fun (_, v) -> v = -1.0)
        let inPoolOutFlow =
            quadValues
            |> List.filter (fun (_, _, v) -> v = 1.0)
        (poolToOutFlow.Length + inPoolOutFlow.Length) = (linValues.Length + quadValues.Length) &&
        poolToOutFlow.Length = 1

    let (|ReductionBilinear|_|) (con : Constraint) =
        if isReductionBilinear con then Some con
        else None

    let isMaterialBalance (con : Constraint) =
        let linValues = linearCoefficients con
        let quadValues = quadraticCoefficients con

        let inpoolFlow = linValues |> List.filter (fun (_, c) -> c = 1.0)
        let pooloutFlow = linValues |> List.filter (fun (_, c) -> c = -1.0)

        (inpoolFlow.Length > 1)
        && (pooloutFlow.Length > 1)
        && (inpoolFlow.Length + pooloutFlow.Length = linValues.Length)
        && (quadValues.IsEmpty)

    let (|MaterialBalance|_|) (con : Constraint) =
        if isMaterialBalance con then Some con
        else None

    let isPathDefinition (con : Constraint) =
        // pathdef: v_ilj - q_il * y_lj = 0
        let linCoeffs = linearCoefficients con
        let quadCoeffs = quadraticCoefficients con
        match (linCoeffs, quadCoeffs, con.UpperBound, con.LowerBound) with
        | ([(_, 1.0)], [(q, y, -1.0)], Bound 0.0, Bound 0.0) when q <> y -> true
        | _ -> false

    let (|PathDefinition|_|) (con : Constraint) =
        if isPathDefinition con then Some con
        else None

    let isFlowCap vByXY (con : Constraint) =
        let isSumOverV vs coefs =
            let cs = coefs |> List.map fst |> Set.ofList
            let overlappingVs = List.map (fun v -> Set.count (Set.intersect cs v)) vs
            let overlaps = (List.sum overlappingVs) > 0
            let atMostOnce = List.forall (fun c -> c <= 1) overlappingVs
            overlaps && atMostOnce

        let coefs = linearCoefficients con
        let vs = List.map Set.ofList vByXY
        let isSum =
            match List.partition (fun (_, c) -> sign(c) < 0) coefs with
            | ([], cs) | ([_], cs) -> isSumOverV vs cs
            | (cs, []) | (cs, [_]) -> isSumOverV vs (List.map (fun (i, c) -> i, abs(c)) cs)
            | _ -> false
        let isUpperBound =
            match con.UpperBound with
            | Bound b when b < 0.0 -> false
            | _ -> true

        con.Kind = UnidentifiedConstraint
        && isSum
        && (List.forall (fun (_, coef) -> abs(coef) = 1.0) (linearCoefficients con))
        && (quadraticCoefficients con |> List.isEmpty)
        && isUpperBound

    let (|FlowCap|_|) vs (con : Constraint) =
        if isFlowCap vs con then Some con
        else None

    let isPoolCapacity vByL (con : Constraint) =
        let correctCoeffs = linearCoefficients con |> List.forall (fun (_, c) -> c = 1.0)
        let coefs = linearCoefficients con |> List.map fst |> Set.ofList

        correctCoeffs && (List.exists (fun v -> (Set.ofList v) = coefs) vByL)

    let (|PoolCapacity|_|) vByL (con : Constraint) =
        if isPoolCapacity vByL con then Some con
        else None

let private pathDefinitionToTuple (con : Constraint) =
    assert Matching.isPathDefinition con
    let v =
        match linearCoefficients con with
        | [(v, _)] -> v
        | _ -> failwith "Constraint %A doesnt appear to be a valid path definition" con
    let (q, y) =
        match quadraticCoefficients con with
        | [(q, y, _)] -> (q, y)
        | _ -> failwith "Constraint %A doesnt appear to be a valid path definition" con
    (v, q, y)


let private pathsGroupedBy groupFun problem =
    problem.Constraints
    // TODO: when tagging path definitions, probably want to use that
    |> List.choose Matching.(|PathDefinition|_|)
    |> List.map pathDefinitionToTuple
    |> Seq.groupBy groupFun
    |> Seq.map (fun (_, paths) -> paths |> Seq.map (fun (v, _, _) -> v) |> List.ofSeq)
    |> List.ofSeq

let private groupByPool (pathDefs : Constraint list) =
    assert (List.forall Matching.isPathDefinition pathDefs)

    let paths = List.map pathDefinitionToTuple pathDefs
    // group paths by pool
    let groupedByPool =
        paths
        |> Seq.groupBy (fun (_, q, _) -> q)
        |> Seq.groupBy (fun (q, paths) -> paths |> Seq.map (fun (_, _, y) -> y) |> Set.ofSeq)
        |> Seq.map (fun (_, xs) -> Seq.map snd xs |> Seq.concat |> List.ofSeq)
        |> List.ofSeq

    groupedByPool

let private vsByPool problem =
    problem.Constraints
    |> List.choose Patterns.(|PathDefinitionConstraint|_|)
    |> groupByPool
    |> List.map (fun paths -> List.map (fun (v, _, _) -> v) paths)

let private pathMapForProblem problem =
    let paths = List.choose Patterns.(|PathDefinitionConstraint|_|) problem.Constraints |> List.map pathDefinitionToTuple
    paths |> List.map (fun (v, q, y) -> ((q, y), v)) |> Map.ofList

module private Build =
    let inputFromConstraint index (con : Constraint) =
        // TODO: replace with real qualities
        match (con.LowerBound, con.UpperBound) with
        | (Bound b, _) | (_, Bound b) -> Input (index,  b, 0.0, [])
        //| _ -> failwith "Bound expected for input constraint"
        | _ -> Input (index, 0.0, 0.0, [])

    let poolFromConstraint index (con : Constraint) =
        match (con.LowerBound, con.UpperBound) with
        | (Bound b, _) | (_, Bound b) -> Pool (index,  b)
        //| _ -> failwith "Bound expected for pool constraint"
        | _ -> Pool (index, 0.0)

    let outputFromConstraint index (con : Constraint) problem =
        // TODO: replace with real qualities
        match List.partition (fun (_, c) -> sign(c) < 0) (linearCoefficients con) with
        | ([], _) | (_, []) ->
            match (con.LowerBound, con.UpperBound) with
            | (Bound b, _) | (_, Bound b) -> Output (index,  b, 0.0, [])
            | _ as e -> failwith "Bound expected for output constraint (%A)" e
        | ([(ofIdx, _)], _) | (_, [(ofIdx, _)]) ->

            match List.tryFind (fun v -> v.Index = ofIdx) problem.Variables with
            | Some {UpperBound = Bound b} ->  Output (index, b, 0.0, [])
            | Some {UpperBound = NoBound} -> Output (index, infinity, 0.0, [])
            | None -> failwith "not an output constraint?"
        | _ -> failwith "expected output constraint"

    let findConstraintWithVar cons varIdx =
        List.tryFind (fun (_, conVars) -> Set.contains varIdx conVars) cons

    let inoutArcForVar inputs outputs varIdx =
        let input =
            match findConstraintWithVar inputs varIdx with
            | Some (inp, _) -> Some inp
            | None -> None
        let output =
            match findConstraintWithVar outputs varIdx with
            | Some (outp, _) -> Some outp
            | None -> None
        match (input, output) with
        | (Some i, Some o) -> [InputOutput(i, o)]
        | _ -> []

    let pathArcsForVar inputs pools outputs varIdx =
        let input =
            match findConstraintWithVar inputs varIdx with
            | Some (inp, _) -> Some inp
            | None -> None
        let pool =
            match findConstraintWithVar pools varIdx with
            | Some (po, _) -> Some po
            | None -> None
        let output =
            match findConstraintWithVar outputs varIdx with
            | Some (outp, _) -> Some outp
            | None -> None

        match (input, pool, output) with
        | (Some i, Some p, Some o) -> [InputPool (i, p); PoolOutput (p, o)]
        | (Some i, Some p, None) -> [InputPool (i, p)]
        | (None, Some p, Some o) -> [PoolOutput (p, o)]
        | _ -> []

    let arcsFromProblem problem =
        // build lists to bind inputs/pools/outputs to their variables
        let inputs =
            problem.Constraints
            |> List.choose (fun con ->
                match con with
                | {Kind = InputCapacityConstraint input; LinearCoefficients = coef} -> Some (input, coef.NonZeroValues |> List.map fst |> Set.ofList)
                | _ -> None)
        let pools =
            problem.Constraints
            |> List.choose (fun con ->
                match con with
                | {Kind = PoolCapacityConstraint pool; LinearCoefficients = coef} -> Some (pool, coef.NonZeroValues |> List.map fst |> Set.ofList)
                | _ -> None)
        let outputs =
            problem.Constraints
            |> List.choose (fun con ->
                match con with
                | {Kind = OutputCapacityConstraint output; LinearCoefficients = coef} -> Some (output, coef.NonZeroValues |> List.map fst |> Set.ofList)
                | _ -> None)

        let inOutFlowVars =
            problem.Variables
            |> List.choose (fun var ->
                match var with
                | {Kind = InOutFlowVariable; Index = idx} -> Some idx
                | _ -> None)
        let pathFlowVars =
            problem.Variables
            |> List.choose (fun var ->
                match var with
                | {Kind = PathVariable; Index = idx} -> Some idx
                | _ -> None)

        let inoutArcs = List.map (inoutArcForVar inputs outputs) inOutFlowVars |> List.concat
        let pathArcs = List.map (pathArcsForVar inputs pools outputs) pathFlowVars |> List.concat
        List.append inoutArcs pathArcs

let private identifyReductions (reductionKind : Constraint -> Constraint option) problem =
    let tagReduction con =
        match reductionKind con with
        | Some con -> {con with Kind = ReductionConstraint}
        | None -> con

    match List.map tagReduction problem.Constraints with
    | xs when xs |> List.choose Patterns.(|ReductionConstraint|_|) |> List.isEmpty -> None
    | taggedCons -> Some {problem with Constraints = taggedCons}

let private tagVariablesFromCapacityConstraints vars cons =
    let allVars =
        cons
        |> List.map (function con -> linearCoefficients con |> List.map (fun (v, _) -> v))
        |> List.concat
        |> Set.ofList

    let knownPathVars =
        vars
        |> List.choose Patterns.(|PathVariable|_|)
        |> List.map (fun {Index = idx} -> idx)
        |> Set.ofList

    let inoutVars = Set.difference allVars knownPathVars

    let tagVariable (var : Variable) =
        match var with
        | {Index = idx} when Set.contains idx inoutVars -> {var with Kind = InOutFlowVariable}
        | _ -> var

    List.map tagVariable vars

let private replacePoolOutFlowWithPath problem (linCoef : Sparse.Vector) =
    let reductionToFlowPathTuple con =
        let linCoef = linearCoefficients con
        let y =
            match List.filter (fun (_, c) -> c = -1.0) linCoef with
            | [(y, _)] -> y
            | _ -> failwith "Expected to find only one y(l,j)"
        let vs = linCoef |> List.filter (fun (_, c) -> c = 1.0) |> List.map fst
        (y, vs)
    let poolOutFlowMap =
        problem.Constraints
        |> List.choose Matching.(|ReductionLinear|_|)
        |> List.map reductionToFlowPathTuple
        |> Map.ofList

    let newLinCoef = Sparse.Vector (Problem.numVariables problem)
    linCoef.NonZeroValues
    |> List.iter (fun (var, coef) ->
        match Map.tryFind var poolOutFlowMap with
        | Some vs -> vs |> List.iter (fun v -> newLinCoef.Item v <- (newLinCoef.Item  v) + coef)
        | None -> newLinCoef.Item var <- (newLinCoef.Item var) + coef)
    newLinCoef

//==- Identify Constraints -==//
let identifyReductionsLinear problem = identifyReductions Matching.(|ReductionLinear|_|) problem

let identifyReductionsBilinear problem = identifyReductions Matching.(|ReductionBilinear|_|) problem

let identifyMaterialBalances problem =
    let tagMaterialBalance = function
        | Matching.MaterialBalance con -> {con with Kind = MaterialBalanceConstraint}
        | _ as con -> con

    match List.map tagMaterialBalance problem.Constraints with
    | xs when xs |> List.choose Patterns.(|MaterialBalanceConstraint|_|) |> List.isEmpty -> None
    | taggedCons -> Some {problem with Constraints = taggedCons}

let identifyPathDefinitions problem =
    // Assume that if reduction constraints are linear then v(i,l,j) must be present
    let tagPathDefinition = function
    | Matching.PathDefinition con -> {con with Kind = PathDefinitionConstraint}
    | _ as con -> con

    let taggedCons = List.map tagPathDefinition problem.Constraints
    Some {problem with Constraints = taggedCons}

let identifyVariablesFromPathDefinitions problem =
    // Tag variables q(i,l), v(i,l,j), y(l,j), note that y(i,j) are not present in these constraints
    let taggedCons = List.choose Patterns.(|PathDefinitionConstraint|_|) problem.Constraints
    let paths = List.map pathDefinitionToTuple taggedCons
    let vs = paths |> List.map (fun (v, _, _) -> v) |> Set.ofList
    let qs = paths |> List.map (fun (_, q, _) -> q) |> Set.ofList
    let ys = paths |> List.map (fun (_, _, y) -> y) |> Set.ofList

    let tagVariable (var : Variable) =
        match var with
        | { Index = idx } when Set.contains idx vs -> {var with Kind = PathVariable}
        | { Index = idx } when Set.contains idx qs -> {var with Kind = FractionFlowVariable}
        | { Index = idx } when Set.contains idx ys -> {var with Kind = PoolOutFlowVariable}
        | _ -> var

    let taggedVars = List.map tagVariable problem.Variables

    match taggedCons with
    | [] -> None
    | taggedCons -> Some {problem with Variables = taggedVars}

// TOOD: use better indexes, not just the constraint #
let identifyOutputCapacities problem =
    let vByIL = pathsGroupedBy (fun (_, q, _) -> q) problem
    // TODO: probably want to change pathsGroupedBy to include a fun to map result
    let yByIL =
        problem.Constraints
        // TODO: when tagging path definitions, probably want to use that
        |> List.choose Matching.(|PathDefinition|_|)
        |> List.map pathDefinitionToTuple
        |> Seq.groupBy (fun (_, q, _) -> q)
        |> Seq.map (fun (_, paths) -> paths |> Seq.map (fun (_, _, y) -> y) |> List.ofSeq)
        |> List.ofSeq


    let tagOutputCapacity i = function
    | Matching.FlowCap vByIL con ->
        {con with Kind = OutputCapacityConstraint (Build.outputFromConstraint i con problem)}
    | Matching.FlowCap yByIL con ->
        {con with Kind = OutputCapacityConstraint (Build.outputFromConstraint i con problem);
                  LinearCoefficients = replacePoolOutFlowWithPath problem con.LinearCoefficients}
    | _ as con -> con

    let taggedCons = List.mapi tagOutputCapacity problem.Constraints
    let outputCapCons = List.choose Patterns.(|OutputCapacityConstraint|_|) taggedCons
    let taggedVars = tagVariablesFromCapacityConstraints problem.Variables outputCapCons

    Some {problem with Constraints = taggedCons; Variables = taggedVars}

let identifyInputCapacities problem =
    let vByLJ = pathsGroupedBy (fun (_, _, y) -> y) problem

    let tagInputCapacity i = function
    | Matching.FlowCap vByLJ con -> {con with Kind = InputCapacityConstraint (Build.inputFromConstraint i con)}
    | _ as con -> con

    let taggedCons = List.mapi tagInputCapacity problem.Constraints
    let taggedVars = tagVariablesFromCapacityConstraints problem.Variables (taggedCons |> List.choose Patterns.(|InputCapacityConstraint|_|))

    Some {problem with Constraints = taggedCons; Variables = taggedVars}

let identifyPoolCapacities problem =
    let vsByPool = vsByPool problem

    let tagPoolCapacity i = function
    | Matching.PoolCapacity vsByPool con -> {con with Kind = PoolCapacityConstraint (Build.poolFromConstraint i con)}
    | _ as con ->
        match {con with LinearCoefficients = replacePoolOutFlowWithPath problem con.LinearCoefficients} with
        | Matching.PoolCapacity vsByPool con -> {con with Kind = PoolCapacityConstraint (Build.poolFromConstraint i con)}
        | _ as con -> con

    let taggedCons = List.mapi tagPoolCapacity problem.Constraints

    Some {problem with Constraints = taggedCons}

let identifyProductQualities problem =
    let hasSameVariables (con : Constraint) (outputCon : Constraint) =
        // remove extra var in epa problems
        let outputVars =
            match List.partition (fun (_, c) -> c < 0.0) (linearCoefficients outputCon) with
            | ([], cs) | ([_], cs) -> cs |> List.map fst |> Set.ofList
            | (cs, []) | (cs, [_]) -> cs |> List.map fst |> Set.ofList
            | _ -> failwith "Not an output constraint"
        let vars = linearCoefficients con |> List.map fst |> Set.ofList
        Set.isSuperset outputVars vars

    let outputCapCons = List.choose Patterns.(|OutputCapacityConstraint|_|) problem.Constraints
    let tagProductQuality (con : Constraint) =
        match con with
        | {Kind = UnidentifiedConstraint; LowerBound = Bound _; UpperBound = NoBound}
        | {Kind = UnidentifiedConstraint; LowerBound = NoBound; UpperBound = Bound _} ->
            match List.tryFind (hasSameVariables con) outputCapCons with
            | Some {Kind = OutputCapacityConstraint(out)} -> {con with Kind = ProductQualityConstraint(out)}
            | Some _ -> failwith "OutputCapacityConstraints with wrong tag"
            | None -> con
        | _ -> con

    Some {problem with Constraints = List.map tagProductQuality problem.Constraints}

//==- Add Constraints -==//
let private addPathDefinitions bilinearTerms problem =
    // add variables v(i,l,j) and constraints v(i,l,j) = q(i,l) * y(l,j)
    let withFlowVariable startIdx i (q, y) = (startIdx + i, q, y)
    let paths =
        bilinearTerms
        |> List.mapi (withFlowVariable (List.length problem.Variables))

    let constraintForPathDef (v, q, y) =
        let numVars = Problem.numVariables problem
        let quadCoef = Sparse.Matrix numVars
        let linCoef = Sparse.Vector numVars
        quadCoef.Item (q, y) <- -1.0
        linCoef.Item v <- 1.0
        {Name = sprintf "PathDef %d = %d %d" v q y;
         QuadraticCoefficients = quadCoef;
         LinearCoefficients = linCoef;
         NonLinear = None;
         UpperBound = Bound 0.0;
         LowerBound = Bound 0.0;
         Kind = PathDefinitionConstraint}

    let vs = paths |> List.map (fun (v, _, _) -> v ) |> Set.ofList
    let newPathVars =
        vs
        |> Set.toList
        |> List.map (fun v ->
            {Name = "v(i,l,j)";
             Index = v;
             UpperBound = NoBound;
             LowerBound = Bound 0.0;
             Kind = PathVariable})
    let newVars = List.concat [newPathVars; problem.Variables]
    let newCons = List.concat [List.map constraintForPathDef paths; problem.Constraints]
    Some {problem with Constraints = newCons; Variables = newVars}

let addPathDefinitionsFromReductions problem =
    let reductions = List.choose Matching.(|ReductionBilinear|_|) problem.Constraints
    let pathsInReduction = quadraticCoefficients >> List.map (fun (q, y, _) -> (q, y))
    let bilinear =
        reductions
        |> List.map pathsInReduction
        |> List.concat
    addPathDefinitions bilinear problem

let addPathDefinitionsFromMaterialBalances (problem, bilinear) =
    addPathDefinitions bilinear problem

let addReductions problem =
    match List.choose Patterns.(|PathDefinitionConstraint|_|) problem.Constraints with
    | [] -> None
    | pathdefs ->
        let pathsInReduction = quadraticCoefficients >> List.map (fun (q, y, _) -> (q, y))
        let numVars = Problem.numVariables problem

        let makeReductionConstraint (y, qs) =
            let linCoef = Sparse.Vector numVars
            let quadCoef = Sparse.Matrix numVars
            linCoef.Item y <- -1.0
            qs |> List.iter (fun q -> quadCoef.Item (q, y) <- 1.0)
            {Name = "Reduction";
             QuadraticCoefficients = quadCoef;
             LinearCoefficients = linCoef;
             NonLinear = None;
             UpperBound = Bound 0.0;
             LowerBound = Bound 0.0;
             Kind = ReductionConstraint}

        let qsForY =
            pathdefs
            |> List.map pathsInReduction
            |> List.concat
            |> Seq.groupBy (fun (q, y) -> y)
            |> Seq.map (fun (y, paths) -> y, paths |> Seq.map fst |> List.ofSeq)
            |> List.ofSeq

        let newCons =
            qsForY
            |> List.map makeReductionConstraint
        Some {problem with Constraints = List.concat [problem.Constraints; newCons]}

let addMissingCapacities problem =
    let addInputCapacities numExisting unusedPaths problem =
        let numVars = Problem.numVariables problem
        let buildInputCapacityConstraint i vs =
            let linCoef = Sparse.Vector (Problem.numVariables problem)
            vs |> List.iter (fun v -> linCoef.Item v <- 1.0)
            {Name = "Input Capacity";
             QuadraticCoefficients = Sparse.Matrix numVars
             LinearCoefficients = linCoef;
             NonLinear = None;
             UpperBound = NoBound;
             LowerBound = Bound 0.0;
             Kind = InputCapacityConstraint (Input (i + numExisting, infinity, 0.0, []))}

        let inputCapacityCons = List.mapi buildInputCapacityConstraint unusedPaths
        {problem with Constraints = List.concat [inputCapacityCons; problem.Constraints]}

    let addPoolCapacities problem =
        let numVars = Problem.numVariables problem
        let buildPoolCapacityConstraint i vs =
            let linCoef = Sparse.Vector (Problem.numVariables problem)
            vs |> List.iter (fun v -> linCoef.Item v <- 1.0)
            {Name = "Pool Capacity";
             QuadraticCoefficients = Sparse.Matrix numVars
             LinearCoefficients = linCoef;
             NonLinear = None;
             UpperBound = NoBound;
             LowerBound = Bound 0.0;
             Kind = PoolCapacityConstraint (Pool (i, infinity))}

        let poolCapacityCons =
            (vsByPool problem)
            |> List.mapi buildPoolCapacityConstraint
        {problem with Constraints = List.concat [poolCapacityCons; problem.Constraints]}

    let addOutputCapacities numExisting unusedPaths problem =
        let numVars = Problem.numVariables problem
        let buildOutputCapacityConstraint i vs =
            let linCoef = Sparse.Vector (Problem.numVariables problem)
            vs |> List.iter (fun v -> linCoef.Item v <- 1.0)
            {Name = "Output Capacity";
             QuadraticCoefficients = Sparse.Matrix numVars
             LinearCoefficients = linCoef;
             NonLinear = None;
             UpperBound = NoBound;
             LowerBound = Bound 0.0;
             Kind = OutputCapacityConstraint (Output (i + numExisting, infinity, 0.0, []))}

        let outputCapacityCons = List.mapi buildOutputCapacityConstraint unusedPaths
        {problem with Constraints = List.concat [outputCapacityCons; problem.Constraints]}

    let inputCons = List.choose Patterns.(|InputCapacityConstraint|_|) problem.Constraints
    let poolCons = List.choose Patterns.(|PoolCapacityConstraint|_|) problem.Constraints
    let outputCons = List.choose Patterns.(|OutputCapacityConstraint|_|) problem.Constraints

    let isPathUnusedInCapacityCons (cons : Constraint list) vs =
        let paths = Set.ofList vs
        cons
        |> List.exists (fun c ->
            let conVars = c.LinearCoefficients.NonZeroValues |> List.map fst |> Set.ofList
            Set.isSubset paths conVars)
        |> not

    let unusedPathsInInputs =
        pathsGroupedBy (fun (_, q, _) -> q) problem
        |> List.filter (isPathUnusedInCapacityCons inputCons)

    let unusedPathsInOutputs =
        pathsGroupedBy (fun (_, _, y) -> y) problem
        |> List.filter (isPathUnusedInCapacityCons outputCons)

    let problemWithPoolCaps =
        match poolCons with
        | [] -> addPoolCapacities problem
        | _ -> problem

    let problemWithAllCapacities =
        problemWithPoolCaps
        |> addInputCapacities (List.length inputCons) unusedPathsInInputs
        |> addOutputCapacities (List.length outputCons) unusedPathsInOutputs

    let inoutVars =
        problem.Variables
        |> List.choose Patterns.(|InOutFlowVariable|_|)
        |> List.map (fun v -> v.Index)
        |> Set.ofList

    let inoutByOutput =
        outputCons
        |> List.map (fun c ->
            let conVars = c.LinearCoefficients.NonZeroValues |> List.map fst |> Set.ofList
            Set.intersect conVars inoutVars)
        |> List.map Set.toList

    let tagInputCapacity i = function
    | Matching.FlowCap inoutByOutput con -> {con with Kind = InputCapacityConstraint (Build.inputFromConstraint i con)}
    | _ as con -> con

    Some {problemWithAllCapacities with Constraints = List.mapi tagInputCapacity problemWithAllCapacities.Constraints}

//==- Replace Variables -==//
let private replaceQuadraticCoefficients (srcQuadCoef : Sparse.Matrix) (srcLinCoef : Sparse.Vector) (pathMap : Map<(int * int), int>) =
    let linCoefs = new Sparse.Vector(srcLinCoef)
    let quadCoefs = Sparse.Matrix (srcQuadCoef.Size)
    srcQuadCoef.NonZeroValues
    |> List.iter (fun (i, j, c) ->
        if pathMap.ContainsKey (i, j) then
            linCoefs.Item pathMap.[(i, j)] <- (linCoefs.Item pathMap.[(i,j)])  + c
        else
            quadCoefs.Item (i, j) <- c)
    (linCoefs, quadCoefs)

let replaceBilinearTerms problem =
    let pathMap = pathMapForProblem problem

    let replaceBilinearTerms = function
    | Matching.PathDefinition con -> con
    | _ as con ->
        let (linCoefs, quadCoefs) = replaceQuadraticCoefficients con.QuadraticCoefficients con.LinearCoefficients pathMap
        {con with LinearCoefficients = linCoefs; QuadraticCoefficients = quadCoefs}

    Some {problem with Constraints = List.map replaceBilinearTerms problem.Constraints}

let replaceBilinearTermsInObjectives problem =
    let pathMap = pathMapForProblem problem

    let replaceBilinearTermsInObjective (obj : Objective) =
        let (linCoefs, quadCoefs) = replaceQuadraticCoefficients obj.QuadraticCoefficients obj.LinearCoefficients pathMap
        {obj with LinearCoefficients = linCoefs; QuadraticCoefficients = quadCoefs}

    Some {problem with Objectives = List.map replaceBilinearTermsInObjective problem.Objectives}

let replacePoolOutFlowWithReductions problem =
    let newCons =
        problem.Constraints
        |> List.map (fun con -> {con with LinearCoefficients = replacePoolOutFlowWithPath problem con.LinearCoefficients})
    Some {problem with Constraints = newCons}

let replaceInPoolWithFraction problem =
    let groupXandYs con =
        let linCoefs = linearCoefficients con
        let xs = List.filter (fun (_, c) -> c = 1.0) linCoefs |> List.map fst
        let ys = List.filter (fun (_, c) -> c = -1.0) linCoefs |> List.map fst
        List.map (fun x -> x, ys) xs

    let addQil numVars idx (x, ys) =
        (x, (numVars + idx), ys)

    let numVars = Problem.numVariables problem
    let xAsFractionSum =
        problem.Constraints
        |> List.choose Patterns.(|MaterialBalanceConstraint|_|)
        |> List.map groupXandYs
        |> List.concat
        |> List.mapi (addQil numVars)

    let fractionVars =
        xAsFractionSum
        |> List.map (fun (_, q, _) -> {Name = "q(i,l)"; Index = q; LowerBound = Bound 0.0; UpperBound = Bound 1.0; Kind = FractionFlowVariable})

    let xAsFractionSumMap = xAsFractionSum |> List.map (fun (x, q, ys) -> (x, (q, ys))) |> Map.ofList

    let replaceVariables con =
        let newLinearCoeffs = Sparse.Vector numVars
        let newQuadraticCoeffs = Sparse.Matrix numVars

        linearCoefficients con
        |> List.iter (fun (v, c) ->
            match Map.tryFind v xAsFractionSumMap with
            | None -> newLinearCoeffs.Item v <- c
            | Some (q, ys) -> ys |> List.iter (fun y -> newQuadraticCoeffs.Item (q, y) <- c))

        {con with LinearCoefficients = newLinearCoeffs; QuadraticCoefficients = newQuadraticCoeffs}

    let newBilinearTerms =
        xAsFractionSum
        |> List.map (fun (_, q, ys) -> ys |> List.map (fun y -> q, y))
        |> List.concat

    let newCons =
        problem.Constraints
        |> List.map replaceVariables
        |> List.choose (fun con ->
            match con.Kind with
            | MaterialBalanceConstraint -> None
            | _ -> Some con)

    // TODO: add simplex constraints
    let newVars = List.concat [problem.Variables; fractionVars]
    Some ({problem with Constraints = newCons; Variables = newVars}, newBilinearTerms)

let groupOutputsUsingProductQualities problem =
    let productQualCons = List.choose Patterns.(|ProductQualityConstraint|_|) problem.Constraints
    let outputCons = List.choose Patterns.(|OutputCapacityConstraint|_|) problem.Constraints

    let containsVariables out prod =
        let getVars c = c |> linearCoefficients |> List.map fst |> Set.ofList
        let outVars = getVars out
        let prodVars = getVars prod
        Set.isSubset outVars prodVars

    let joinConstraints (pq : Constraint, (cons : Constraint list)) =
        let quadCoef = Sparse.Matrix (Problem.numVariables problem)
        let linCoef = Sparse.Vector (Problem.numVariables problem)
        pq.LinearCoefficients.NonZeroValues |> List.iter (fun (i, c) -> linCoef.Item i <- 1.0)

        {Name = "Output Capacity";
         QuadraticCoefficients = quadCoef;
         LinearCoefficients = linCoef;
         NonLinear = None;
         UpperBound = NoBound;
         LowerBound = Bound 0.0;
         Kind = (cons.Head.Kind)}

    let outputCapFromProdQual i (pqCons : Constraint) =
        let quadCoef = Sparse.Matrix (Problem.numVariables problem)
        let linCoef = Sparse.Vector (Problem.numVariables problem)
        pqCons.LinearCoefficients.NonZeroValues |> List.iter (fun (i, c) -> linCoef.Item i <- 1.0)
        {Name = "Output Capacity";
         QuadraticCoefficients = quadCoef;
         LinearCoefficients = linCoef;
         NonLinear = None;
         UpperBound = NoBound;
         LowerBound = Bound 0.0;
         Kind = OutputCapacityConstraint (Output(i, infinity, 0.0, []))}

    let newOutCons =
        productQualCons
        //|> List.mapi outputCapFromProdQual
        |> List.map (fun pq -> pq, List.filter (fun o -> containsVariables o pq) outputCons)
        |> List.map joinConstraints
        //|> Seq.groupBy (fun {Kind = OutputCapacityConstraint o} -> o)
        //|> Seq.map (fun (_, cons) -> Seq.head cons)
        //|> Seq.toList

    let consNoOut = List.choose (fun c ->
        match c with
        | Patterns.OutputCapacityConstraint c -> None
        | _ as c -> Some c) problem.Constraints
    Some {problem with Constraints = (newOutCons @ consNoOut)}

/// compute qualities form the given product quality constraints
let computeQualities problem =

    let inputVariables =
        problem.Constraints
        |> List.choose (fun con ->
            match con with
            | {Kind = InputCapacityConstraint(Input(index = i)); LinearCoefficients = coef} ->
                Some (i, coef.NonZeroValues |> List.map fst |> Set.ofList)
            | _ ->
                None)

    let outputVariables =
        problem.Constraints
        |> List.choose (fun con ->
            match con with
            | {Kind = OutputCapacityConstraint(Output(index = j)); LinearCoefficients = coef} ->
                Some(j, coef.NonZeroValues |> List.map fst |> Set.ofList)
            | _ ->
                None)

    let findIndex v vars =
        match List.tryFind (fun (_, vs) -> Set.contains v vs) vars with
        | Some (idx, _) -> idx
        | None -> failwith "Index not found"

    let productQualityDifference k (con : Constraint) =
        let coefs = con.LinearCoefficients.NonZeroValues

        coefs
        |> List.map (fun (v, c) ->
            let inputIdx = findIndex v inputVariables
            let outputIdx = findIndex v outputVariables
            (inputIdx, outputIdx, k, c))


    let productQualityDifferences (cons : Constraint list) =
        cons
        |> Seq.groupBy (fun {Kind = ProductQualityConstraint o} -> o)
        |> Seq.map (fun (_, cs) -> Seq.mapi productQualityDifference cs |> List.concat)
        |> Seq.concat
        |> Set.ofSeq
        |> Set.toSeq

    let (differencesU, differencesL) =
        problem.Constraints
        |> List.choose Patterns.(|ProductQualityConstraint|_|)
        |> List.partition (fun con ->
            match (con.LowerBound, con.UpperBound) with
            | (Bound _, NoBound) -> false
            | _ -> true)
        |> (fun (u, l) -> (productQualityDifferences u, productQualityDifferences l))

    let solver = ProductSolver()
    let (inSpec, prodSpec) = solver.Solve(differencesU, differencesL)

    (inSpec, prodSpec)

/// Extract a pooling problem network from the optimization problem
let extractNetwork normProblem =
    let inputCapacityCons = List.choose Patterns.(|InputCapacityConstraint|_|) normProblem.Constraints
    if List.isEmpty inputCapacityCons then
        failwith "Input constraints needed"

    let poolCapacityCons = List.choose Patterns.(|PoolCapacityConstraint|_|) normProblem.Constraints
    if List.isEmpty poolCapacityCons then
        failwith "Pool constraints needed"

    let outputCapacityCons = List.choose Patterns.(|OutputCapacityConstraint|_|) normProblem.Constraints
    if List.isEmpty outputCapacityCons then
        failwith "Output constraints needed"

    // Start building the network
    let inputFromConstraint (obj : Objective) (con : Constraint) =
        match con with
        | {Kind = InputCapacityConstraint
                    (Input(index = idx; capacity = cap; spec = spec));
           LinearCoefficients = linCoefs} ->
            let flowVarCost = obj.LinearCoefficients.NonZeroValues |> List.filter (fun (_, c) -> c > 0.0) |> Map.ofList
            let coefForVar (v, _) = Map.tryFind v flowVarCost
            let cost =
                match List.tryPick coefForVar linCoefs.NonZeroValues with
                | Some c -> c
                | None -> 0.0
            Input(idx, cap, cost, spec)
        | _ -> failwith "InputCapacityConstraint expected"

    let poolFromConstraint (con : Constraint) =
        match con.Kind with
        | PoolCapacityConstraint pool -> pool
        | _ -> failwith "PoolCapacityConstraint expected"

    let pathToOutFlow =
        normProblem.Constraints
        |> List.choose Patterns.(|PathDefinitionConstraint|_|)
        |> List.map pathDefinitionToTuple
        |> List.map (fun (v, _, y) -> v, y)
        |> Map.ofList
    let outputFromConstraint (obj : Objective) (con : Constraint) =
        match con with
        | {Kind = OutputCapacityConstraint (Output(index = idx; capacity = cap; spec = spec)); LinearCoefficients = linCoefs} ->
            let outFlowVar (v, c) =
                match pathToOutFlow.TryFind v with
                | Some y -> y, c
                | None -> v, c
            let profitForFlow = obj.LinearCoefficients.NonZeroValues |> List.filter (fun (_, c) -> c < 0.0) |> Map.ofList
            let profit =
                match linCoefs.NonZeroValues |> List.map outFlowVar |> List.tryPick (fun (v, _) -> profitForFlow.TryFind v) with
                | Some p -> p
                | None -> 0.0
            Output(idx, cap, abs(profit), spec)
        | _ -> failwith "OutputCapacityConstraint expected"

    let (inputQualities, outputQualities) =
        try
            computeQualities normProblem
        with
            | _ -> ([], [])

    let inputs =
        inputCapacityCons
        |> List.map (inputFromConstraint normProblem.Objectives.Head)

    let inputsWithSpec =
        if (List.length inputs) = (List.length inputQualities) then
            inputs
            |> List.map2 (fun qual (Input(idx, cap, pr, _)) -> Input(idx, cap, pr, qual)) inputQualities
        else
            inputs
    let pools = List.map poolFromConstraint poolCapacityCons

    let outputs =
        outputCapacityCons
        |> List.map (outputFromConstraint normProblem.Objectives.Head)

    let outputsWithSpec =
        if (List.length outputs) = (List.length outputQualities) then
            outputs
            |> List.map2 (fun qual (Output(idx, cap, pr, _)) -> Output(idx, cap, pr, qual)) outputQualities
        else
            outputs

    let arcs = Build.arcsFromProblem normProblem |> Set.ofList |> Set.toList

    let network = Network.create inputsWithSpec pools outputsWithSpec arcs

    let remainingConstraints =
        normProblem.Constraints
        |> List.choose Patterns.(|UnidentifiedConstraint|_|)
        |> List.map (fun con -> {con with Kind = OtherConstraint})

    let newProblem = Problem.create normProblem.Variables normProblem.Objectives remainingConstraints
    {newProblem with PoolingNetwork = Some network}
