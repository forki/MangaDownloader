open MangaDownloader
open FSharp.Data


// Imported Types
type Int32 = System.Int32
type Uri   = System.Uri
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
type Chapter with
    member o.Title = chapterTitle o
type Page with
    member o.Number = pageNumber o


// Fetching/Parsing Manga
let getManga (uri:string) = maybe {
    let! html = Download.asHtml uri
    let name =
        html.Descendants "div"
        |> Seq.filter  (HtmlNode.hasId "mangaproperties")
        |> Seq.collect (fun node -> node.Descendants "h2")
        |> Seq.map     (fun node -> node.InnerText())
        |> Seq.head
    return Manga (name, Uri uri)
}

let getChapters (Manga (name,uri)) = maybe {
    let! html = Download.asHtml uri
    return 
        html.Descendants "div"
        |> Seq.filter  (HtmlNode.hasId "chapterlist")
        |> Seq.collect (fun node -> node.Descendants "a")
        |> Seq.map     (fun node ->
            let title  = node.InnerText()
            let _,href = Uri.TryCreate(uri, node.AttributeValue("href"))
            Chapter (title, href)
        )
}

let getPages (Chapter (chapter,uri)) = maybe {
    let! html = Download.asHtml uri
    return
        html.Descendants "select"
        |> Seq.filter  (fun node -> node.HasId "pageMenu" )
        |> Seq.collect (fun node -> node.Descendants "option")
        |> Seq.map (fun node ->
            let pageNumber = node.InnerText() |> System.Int32.Parse
            let _,href     = Uri.TryCreate(uri, node.AttributeValue("value"))
            Page (pageNumber, href)
        )
}

let getImage (Page (no,uri)) = maybe {
    let! html = Download.asHtml uri
    return!
        html.Descendants "div"
        |> Seq.filter  (fun node -> node.HasId "imgholder" )
        |> Seq.collect (fun node -> node.Descendants "img")
        |> Seq.tryHead
        |> Option.map (fun node -> node.AttributeValue "src")
        |> Option.map (Uri >> Image)
}

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

    let downloadChapter manga no = maybe {
        let! no           = Int32.tryParse no
        let! manga        = getManga manga
        let! chapters     = getChapters manga
        let! chapter      = Seq.tryItem (no-1) chapters
        let! pages        = getPages chapter
        for page in pages do
            printfn "Downloading [%s] Page %d" chapter.Title page.Number
            let! image = getImage page
            use file   = getFileHandle manga.Title chapter.Title page.Number image
            do! retryDownload image file
    }

    let downloadChapters manga start ``end`` = maybe {
        let decrease str = 
            Int32.tryParse str |> Option.map (fun x -> x - 1)
        let! start    = decrease start
        let! ``end``  = decrease ``end``
        let! manga    = getManga manga
        let! chapters = getChapters manga
        for i in start .. ``end`` do
            let! chapter      = Seq.tryItem i chapters
            let! pages = getPages chapter
            for page in pages do
                printfn "Downloading [%s] Page %d" chapter.Title page.Number
                let! image = getImage page
                use  file  = getFileHandle manga.Title chapter.Title page.Number image
                do!  retryDownload image file
    }

[<EntryPoint>]
let main argv =
    match argv with
    | [| manga |]       -> Console.showChapters manga
    | [| manga; no |]   -> Console.downloadChapter manga no |> ignore
    | [| manga; s; e |] -> Console.downloadChapters manga s e |> ignore
    | _                 -> Console.showUsage ()
    0 // return an integer exit code
