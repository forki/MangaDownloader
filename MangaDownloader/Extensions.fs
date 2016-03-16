namespace MangaDownloader

[<AutoOpen>]
module Extensions =
    type System.Int32 with
        static member tryParse str =
            match System.Int32.TryParse(str) with
            | false,_ -> None
            | true,x  -> Some x
