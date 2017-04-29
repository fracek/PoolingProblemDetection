namespace Pooling

// TODO: make code more robust by checking indexes

module Sparse =
    [<StructuredFormatDisplay("{_ToString}")>]
    type Matrix = class
        // TODO: efficient implementation as in http://web.eecs.utk.edu/~dongarra/etemplates/node373.html
        val mutable private values : (int * int * double) array
        val private size : int

        new (size : int) =
            { values = Array.empty;
              size = size }

        new (size : int, values : (int * int * double) list) =
            { values = Array.ofList values;
              size = size }

        new (v : Matrix) =
            { values = Array.ofList v.NonZeroValues;
              size = v.Size }

        member private this.getItem i j =
            match Array.tryFind (fun (ii, jj, _) -> ii = i && jj = j) this.values with
            | Some (_, _, v) -> v
            | None -> 0.0

        member private this.setItem i j value =
            let (lt, gt) = Array.partition (fun (ii, jj, _) -> ii < i && jj < j) this.values
            this.values <- Array.concat [lt; [|(i, j, value)|]; gt]

        member this.Size = this.size

        member this.NonZero = Array.length this.values

        member this.NonZeroValues = List.ofArray this.values

        member this.Item
            with get (i : int, j : int) = this.getItem i j
            and  set (i : int, j : int) (value : double) = this.setItem i j value

        member this._ToString =
            sprintf "Matrix(Size = %A, NonZero = %A)" this.Size this.NonZero
    end

    [<StructuredFormatDisplay("{_ToString}")>]
    type Vector = class
        val mutable private values : (int * double) array
        val private size : int

        new (size : int) =
            { values = Array.empty;
              size = size }

        new (size : int, values : (int * double) list) =
            { values = Array.ofList values;
              size = size }

        new (v : Vector) =
            { values = Array.ofList v.NonZeroValues;
              size = v.Size }

        member private this.getItem idx =
            match Array.tryFind (fun (i, _) -> i = idx) this.values with
            | Some (_, v) -> v
            | None -> 0.0

        member private this.insertValue idx value =
            let (lt, gt) = Array.partition (fun (i, _) -> i < idx) this.values
            this.values <- Array.concat [lt; [|(idx, value)|]; gt]

        member private this.setItem idx value =
            match Array.tryFindIndex (fun (i, _) -> i = idx) this.values with
            | Some i -> this.values.[i] <- (idx, value)
            | None -> this.insertValue idx value

        member this.Size = this.size

        member this.NonZero = Array.length this.values

        member this.NonZeroValues = List.ofArray this.values

        member this.Item
            with get (i : int) = this.getItem i
            and  set (i : int) (value : double) = this.setItem i value

        member this._ToString =
            sprintf "Vector(Size = %A, NonZero = %A)" this.Size this.NonZero
    end
