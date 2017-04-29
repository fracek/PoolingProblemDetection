namespace Pooling.OSiL
open System
open System.IO
open System.Xml.Linq

type OSiL =
    static member Load (path : string) =
        use stream = new StreamReader(path)
        let doc = XDocument.Load(stream)
        Pooling.OSiL.Parser.parseOSiL doc
