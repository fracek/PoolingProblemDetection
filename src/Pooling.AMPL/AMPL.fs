namespace Pooling.AMPL

open System
open System.IO

open Pooling
open Pooling.AMPL.Data

type AMPL =
    static member WriteDataFile(network: Network.T, writer: TextWriter) =
        formatData network writer
