namespace MangaDownloader

module Retry =
    let fromException x f y =
        let rec loop count =
            try
                Some <| f y
            with
                | exn ->
                    match count < x with
                    | true  -> loop (count+1)
                    | false -> 
                        printfn "Error: %A" exn
                        None
        loop 0
