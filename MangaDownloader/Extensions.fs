namespace MangaDownloader

// Imported Types
type Uri   = System.Uri
type Int32 = System.Int32
type File  = System.IO.File
type Dir   = System.IO.Directory
type Path  = System.IO.Path
type Regex = System.Text.RegularExpressions.Regex
type WebRequest  = System.Net.HttpWebRequest
type IDisposable = System.IDisposable

// Extensions
module Seq =
    let filterIndexed f = Seq.indexed >> Seq.filter f >> Seq.map snd

module Option =
    let apply fx xx =
        match fx,xx with
        | Some f, Some x -> Some (f x)
        | Some _, None _ -> None
        | None _, Some _ -> None
        | None _, None _ -> None

    let (<*>) = apply
       
    let traverse f sequence =
        let cons x s    = seq { yield! s; yield x }
        let cons x s    = Some cons <*> x <*> s
        let folder xs x = cons (f x) xs
        Seq.fold folder (Some Seq.empty) sequence

    let sequence s = traverse id s

[<AutoOpen>]
module Extensions =
    type System.Int32 with
        static member tryParse str =
            match System.Int32.TryParse(str) with
            | false,_ -> None
            | true,x  -> Some x

    type System.Uri with
        static member tryCreate (root:System.Uri) (rel:string) =
            match System.Uri.TryCreate(root, rel) with
            | false,_ -> None
            | true,x  -> Some x