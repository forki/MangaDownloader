namespace MangaDownloader

type Uri = System.Uri

module Option =
    let apply fx xx =
        match fx,xx with
        | Some f, Some x -> Some (f x)
        | Some _, None _ -> None
        | None _, Some _ -> None
        | None _, None _ -> None
       
    let traverse f (sequence:seq<_>) =
        let cons (x:'a) (s:seq<'a>) = seq { yield! s; yield x; }
        let cons x s =
            let cons = Option.map cons x
            (apply cons) s
        let folder xs x =
            cons (f x) xs
        Seq.fold folder (Some(Seq.empty)) sequence

    let sequence (sequence:seq<_>) =
        traverse id sequence

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