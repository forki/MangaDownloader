open MangaDownloader
open Argu

let (>>=) m f = Result.bind f m

// Downloads the chapters of a manga and in case of an error print it to the console
let fromTo uri start stop =
    let result = Manga.fromUri uri >>= Manga.downloadFromTo start stop
    match result with
    | Ok _      -> ()
    | Error err -> printfn "%s" (Error.toString err)

// Shows all Chapters of a Manga
let showChapters uri =
    Manga.fromUri uri >>= Manga.chapters |> (fun chapters ->
        match chapters with
        | Error err   -> printfn "Error: Couldn't fetch Manga chapters: %s" (Error.toString err)
        | Ok chapters -> 
            chapters |> Seq.iteri (fun i chapter ->
                printfn "%4d: %s" (i+1) chapter.Title
            )
    )

// The CLI Arguments
type Arguments =
    | Uri   of string
    | Only  of int
    | [<PrintLabels>] Range of start:int * stop:int
    | Start of int
    | All
with
    interface IArgParserTemplate with
        member s.Usage =
            match s with
            | Uri _   -> "uri for the manga to download"
            | Only _  -> "downloads specified chapter"
            | Range _ -> "specify the chapters to download"
            | Start _ -> "starts downloading from specified chapter"
            | All _   -> "downloads all chapters"

[<EntryPoint>]
let main argv =
    // Creating Argument Parser
    let parser = ArgumentParser.Create<Arguments>()
    let cli    = parser.Parse argv

    // Subtracting "-1" from every int because "showChapters" shows
    // all chapters "1" based instead of zero based. But the numbers
    // are passed to sequences that are zero-based
    if cli.Contains <@ Uri @> then
        // fetch --uri and convert it into a Uri object
        let uri = cli.PostProcessResult(<@ Uri @>, (fun x -> new Uri(x)))

        if cli.Contains <@ All @> then
            fromTo uri 0 Int32.MaxValue
        elif cli.Contains <@ Only @> then
            let no = cli.GetResult <@ Only @> - 1
            fromTo uri no no
        elif cli.Contains <@ Range @> then
            let start,stop = cli.GetResult <@ Range @>
            fromTo uri (start-1) (stop-1)
        elif cli.Contains <@ Start @> then
            let start = cli.GetResult <@ Start @> - 1
            fromTo uri start Int32.MaxValue
        else
            showChapters uri
    else 
        printfn "%s" <| parser.Usage()
    0 // return an integer exit code
    