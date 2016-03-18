open MangaDownloader

// Fetching Manga informations
let (>>=) m f   = Option.bind f m
let (>->) f g x = (f x) >>= g

// CLI Handling
module Console =
    let showUsage () =
        printfn "MangaDownloader.exe [MangaUrl]"
        printfn "MangaDownloader.exe [MangaUrl] all"
        printfn "MangaDownloader.exe [MangaUrl] [chapter]"
        printfn "MangaDownloader.exe [MangaUrl] [from] [to]"

    let showChapters manga =
        Manga.getManga manga |> Option.iter (fun manga ->
            printfn "%s" manga.Title
            match Manga.getChapters manga with
            | None          -> printfn "Error: Couldn't fetch Chapters"
            | Some chapters ->
                chapters |> Seq.iteri (fun i chapter ->
                    printfn "%d: %s" (i+1) chapter.Title
                )
        )

    let downloadChapter (chapter:Chapter) =
        chapter |> Chapter.getPages >>= Option.traverse Page.download |> Option.map ignore

    let between x y i =
        i >= x && i <= y

    let downloadFromTo manga start stop = maybe {
        let! chapters = Manga.getManga manga >>= Manga.getChapters |> Option.map (Seq.filterIndexed (fst >> between start stop))
        for chapter in chapters do
            do! downloadChapter chapter
    }

[<EntryPoint>]
let main argv =
    let (|Int|_|) = Int32.tryParse

    // Subtracting "-1" from every "no" because in "showChapters" all chapters are shown "1" based 
    // instead of zero based.
    match argv with
    | [|manga|]         -> Console.showChapters manga
    | [|manga; "all"|]  -> Console.downloadFromTo manga 0 Int32.MaxValue |> ignore
    | [|manga; Int no|] -> Console.downloadFromTo manga (no-1) (no-1) |> ignore
    | [|manga; Int no; "end"|]       -> Console.downloadFromTo manga (no-1) Int32.MaxValue |> ignore
    | [|manga; Int start; Int stop|] -> Console.downloadFromTo manga (start-1) (stop-1) |> ignore
    | _ -> Console.showUsage ()
    0 // return an integer exit code
