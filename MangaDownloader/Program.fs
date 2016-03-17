open MangaDownloader
open FSharp.Data


// Imported Types
type Int32 = System.Int32
type File  = System.IO.File
type Dir   = System.IO.Directory
type Path  = System.IO.Path
type Regex = System.Text.RegularExpressions.Regex
type WebRequest  = System.Net.HttpWebRequest


// Manga Types
type Image   = Image   of Uri
type Page    = Page    of int * Uri
type Chapter = Chapter of string * Uri
type Manga   = Manga   of string * Uri

let mangaTitle   (Manga   (title,_)) = title
let chapterTitle (Chapter (title,_)) = title
let pageNumber   (Page    (no,_))    = no

type Manga with
    member o.Title = mangaTitle o
    static member create name uri = Manga(name,uri)
type Chapter with
    member o.Title = chapterTitle o
type Page with
    member o.Number = pageNumber o
type Image with
    static member create uri = Image uri


// Fetching Manga 
let (>>=) m f = Option.bind f m
let (>->) f g x = (f x) >>= g

let getManga uri =
    let uri = Uri uri
    uri |>  Download.asHtml >>= Manga.extractTitle |> Option.map (fun name -> Manga.create name uri)

let getChapters (Manga (name,uri)) = maybe {
    let! html = Download.asHtml uri
    return!
        Manga.extractChapters html
        |> Option.traverse (fun (title,href) ->
            Uri.tryCreate uri href |> Option.map (fun url -> Chapter(title,url))
        )
}

let getPages (Chapter (chapter,uri)) = maybe {
    let! html  = Download.asHtml uri
    let! pages = Manga.extractPages html
    return!
        pages |> Option.traverse (fun (pageNumber,href) ->
            Uri.tryCreate uri href |> Option.map (fun url -> Page(pageNumber,url))
        )
}

let getImage (Page (no,uri)) =
    uri |> Download.asHtml >>= Manga.extractImage |> Option.map Image.create

let fileExtension fileName =
    Regex(".*\.(.*)$").Match(fileName).Groups.[1].Value

let getFileHandle manga chapter pageNo (Image uri) =
    let ext      = uri.Segments |> Array.last |> fileExtension
    let fileName = sprintf "%d.%s" pageNo ext
    let path     = Path.Combine(manga, chapter, fileName)
    path |> Path.GetDirectoryName |> Dir.CreateDirectory |> ignore
    File.Open(path, System.IO.FileMode.Append, System.IO.FileAccess.Write)

let download (Image uri) (file:System.IO.FileStream) = maybe {
    let! uriSize = Download.size uri
    if file.Position < uriSize then
        let req = Download.getRequest uri
        req.AddRange(file.Position)
        let stream = req |> Download.asStream
        stream.CopyTo(file)
        return ()
}

let retryDownload image file = maybe {
    let! result = (download image |> Retry.fromException 5) file
    return! result
}


// CLI Handling
module Console =
    let showUsage () =
        printfn "MangaDownloader.exe [MangaUrl]"
        printfn "MangaDownloader.exe [MangaUrl] all"
        printfn "MangaDownloader.exe [MangaUrl] [chapter]"
        printfn "MangaDownloader.exe [MangaUrl] [from] [to]"

    let showChapters manga =
        getManga manga |> Option.iter (fun manga ->
            let (Manga (title,_)) = manga
            printfn "%s" title
            match getChapters manga with
            | None          -> printfn "Error: Couldn't fetch Chapters"
            | Some chapters ->
                chapters |> Seq.iteri (fun i (Chapter (chapter,_)) ->
                    printfn "%d: %s" (i+1) chapter
                )
        )

    let downloadPage (manga:Manga) (chapter:Chapter) (page:Page) = maybe {
        printfn "Downloading [%s] Page %d" chapter.Title page.Number
        let! image = getImage page
        use file   = getFileHandle manga.Title chapter.Title page.Number image
        do! retryDownload image file
    }

    let downloadChapter (manga:Manga) (chapter:Chapter) = maybe {
        let! pages = getPages chapter
        for page in pages do
            do! downloadPage manga chapter page
    }

    let decrease str = 
            Int32.tryParse str |> Option.map (fun x -> x - 1)

    let downloadSingle manga no = maybe {
        let! no           = decrease no
        let! manga        = getManga manga
        let! chapters     = getChapters manga
        do! chapters |> Seq.tryItem no >>= downloadChapter manga
    }

    let downloadMulti manga start ``end`` = maybe {
        let! start    = decrease start
        let! ``end``  = decrease ``end``
        let! manga    = getManga manga
        let! chapters = getChapters manga
        for no in start .. ``end`` do
            do! chapters |> Seq.tryItem no >>= downloadChapter manga
    }

[<EntryPoint>]
let main argv =
    match argv with
    | [| manga |]       -> Console.showChapters manga
    | [| manga; no |]   -> Console.downloadSingle manga no |> ignore
    | [| manga; s; e |] -> Console.downloadMulti manga s e |> ignore
    | _                 -> Console.showUsage ()
    0 // return an integer exit code
