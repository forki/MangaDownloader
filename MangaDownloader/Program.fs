open MangaDownloader

let (>>=) m f = Option.bind f m

let showUsage () =
    printfn "MangaDownloader.exe [MangaUrl]"
    printfn "MangaDownloader.exe [MangaUrl] all"
    printfn "MangaDownloader.exe [MangaUrl] [chapter]"
    printfn "MangaDownloader.exe [MangaUrl] [from] [to]"

let showChapters uri =
    Manga.fromUri uri >>= Manga.chapters |> (fun chapters ->
        match chapters with
        | None          -> printfn "Error: Couldn't fetch Manga chapters"
        | Some chapters -> 
            chapters |> Seq.iteri (fun i chapter ->
                printfn "%4d: %s" (i+1) chapter.Title
            )
    )

[<EntryPoint>]
let main argv =
    let (|Uri|_|) input = try Some(new Uri(input)) with exn -> None
    let (|Int|_|) = Int32.tryParse

    // Subtracting "-1" from every "no" because in "showChapters" all chapters are shown "1" based 
    // instead of zero based.
    match argv with
    | [|Uri uri|]         -> showChapters uri
    | [|Uri uri; "all"|]  -> Manga.fromUri uri >>= Manga.downloadFromTo 0 Int32.MaxValue |> ignore
    | [|Uri uri; Int no|] -> Manga.fromUri uri >>= Manga.downloadFromTo (no-1) (no-1)    |> ignore
    | [|Uri uri; Int no; "end"|] -> 
        Manga.fromUri uri >>= Manga.downloadFromTo (no-1) Int32.MaxValue |> ignore
    | [|Uri uri; Int start; Int stop|] -> 
        Manga.fromUri uri >>= Manga.downloadFromTo (start-1) (stop-1) |> ignore
    | _ -> showUsage ()
    0 // return an integer exit code
