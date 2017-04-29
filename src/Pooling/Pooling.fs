namespace Pooling

type Input = Input of index: int * capacity: double *
                      price: double * spec: double list
type Pool = Pool of index: int * capacity: double
type Output = Output of index: int * capacity: double *
                        price: double * spec: ((* min *)double * (* max *)double) list
type Arc =
    | PoolOutput of Pool * Output
    | InputOutput of Input * Output
    | InputPool of Input * Pool

type Bound =
    | NoBound
    | Bound of double

type ConstraintKind =
    | PathDefinitionConstraint
    | ReductionConstraint
    | MaterialBalanceConstraint
    | InputCapacityConstraint of Input
    | PoolCapacityConstraint of Pool
    | OutputCapacityConstraint of Output
    | ProductQualityConstraint of Output
    | OtherConstraint
    | UnidentifiedConstraint

type Constraint =
    {
    Name: string;
    QuadraticCoefficients: Sparse.Matrix;
    LinearCoefficients: Sparse.Vector;
    NonLinear: Nl.Expr option;
    UpperBound: Bound;
    LowerBound: Bound;
    Kind: ConstraintKind;
    }

type ObjectiveKind =
    | ObjectiveMax
    | ObjectiveMin

type Objective =
    {
    Kind: ObjectiveKind;
    QuadraticCoefficients: Sparse.Matrix;
    LinearCoefficients: Sparse.Vector;
    }

type VariableKind =
    | FractionFlowVariable // q(i,l)
    | PoolOutFlowVariable  // y(l,j)
    | InOutFlowVariable    // y(i,j)
    | PathVariable         // v(i,l,j)
    | OtherVariable

type Variable =
    {
    Name: string;
    Index: int;
    UpperBound: Bound;
    LowerBound: Bound;
    Kind: VariableKind;
    }

module Network =
    type T =
        {
        Inputs: Input list;
        Pools: Pool list;
        Outputs: Output list;
        Arcs: Arc list;
        }

    let numSpecs network =
        let inspecs = network.Inputs |> List.map (fun (Input (spec = s)) -> List.length s)
        let outspecs = network.Outputs |> List.map (fun (Output (spec = s)) -> List.length s)
        assert (List.min inspecs = List.max inspecs)
        assert (List.min inspecs = List.min outspecs)
        assert (List.min outspecs = List.max outspecs)
        List.min inspecs

    let create inputs pools outputs arcs =
        {
        Inputs = inputs;
        Pools = pools;
        Outputs = outputs;
        Arcs = arcs;
        }

/// Active patterns for constraints and variables
module Patterns =
    // Constraints
    let (|ReductionConstraint|_|) (con : Constraint) =
        match con.Kind with
        | ReductionConstraint -> Some con
        | _ -> None

    let (|PathDefinitionConstraint|_|) (con : Constraint) =
        match con.Kind with
        | PathDefinitionConstraint -> Some con
        | _ -> None

    let (|MaterialBalanceConstraint|_|) (con : Constraint) =
        match con.Kind with
        | MaterialBalanceConstraint -> Some con
        | _ -> None

    let (|InputCapacityConstraint|_|) (con : Constraint) =
        match con.Kind with
        | InputCapacityConstraint _ -> Some con
        | _ -> None

    let (|PoolCapacityConstraint|_|) (con : Constraint) =
        match con.Kind with
        | PoolCapacityConstraint _ -> Some con
        | _ -> None

    let (|OutputCapacityConstraint|_|) (con : Constraint) =
        match con.Kind with
        | OutputCapacityConstraint _ -> Some con
        | _ -> None

    let (|ProductQualityConstraint|_|) (con : Constraint) =
        match con.Kind with
        | ProductQualityConstraint _ -> Some con
        | _ -> None

    let (|OtherConstraint|_|) (con : Constraint) =
        match con.Kind with
        | OtherConstraint -> Some con
        | _ -> None

    let (|UnidentifiedConstraint|_|) (con : Constraint) =
        match con.Kind with
        | UnidentifiedConstraint -> Some con
        | _ -> None

    // variables
    let (|FractionFlowVariable|_|) (var : Variable) =
        match var.Kind with
        | FractionFlowVariable -> Some var
        | _ -> None

    let (|PoolOutFlowVariable|_|) (var : Variable) =
        match var.Kind with
        | PoolOutFlowVariable -> Some var
        | _ -> None

    let (|InOutFlowVariable|_|) (var : Variable) =
        match var.Kind with
        | InOutFlowVariable -> Some var
        | _ -> None

    let (|PathVariable|_|) (var : Variable) =
        match var.Kind with
        | PathVariable -> Some var
        | _ -> None

    let (|OtherVariable|_|) (var : Variable) =
        match var.Kind with
        | OtherVariable -> Some var
        | _ -> None

module Problem =
    type T =
        {
        Variables: Variable list;
        Objectives: Objective list;
        PoolingNetwork: Network.T option;
        Constraints: Constraint list;
        }

    let create variables objectives constraints =
        {
        Variables = variables;
        Objectives = objectives;
        PoolingNetwork = None;
        Constraints = constraints;
        }

    let numVariables problem =
        List.length problem.Variables

    let numConstraints problem =
        List.length problem.Constraints

    let reductions problem =
        List.choose Patterns.(|ReductionConstraint|_|) problem.Constraints

    let pathDefinitions problem =
        List.choose Patterns.(|PathDefinitionConstraint|_|) problem.Constraints
