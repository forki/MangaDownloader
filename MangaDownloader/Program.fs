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
type Manga = {
    Title: string
    Uri:   Uri
}
with static member create title uri = {Title=title; Uri=uri}

type Chapter = {
    Manga: Manga
    Title: string
    Uri:   Uri
}
with static member create manga title uri = {Manga=manga; Title=title; Uri=uri}

type Page = {
    Chapter: Chapter
    Number:  int
    Uri:     Uri
}
with static member create chapter no uri = {Chapter=chapter; Number=no; Uri=uri}

type Image = {
    Page: Page
    Uri:  Uri
}
with static member create page uri = {Page=page; Uri=uri}

type Manga with
    member o.createChapter = Chapter.create o
    member o.createChapterWithRelativeUri(title,str) = 
        str |> Uri.tryCreate o.Uri |> Option.map (o.createChapter title)
type Chapter with
    member o.createPage = Page.create o
    member o.createPageWithRelativeUri(no,str) =
        str |> Uri.tryCreate o.Uri |> Option.map (o.createPage no)
type Page with
    member o.createImage = Image.create o


// Fetching Manga informations
let (>>=) m f = Option.bind f m
let (>->) f g x = (f x) >>= g

let getManga uri =
    let uri = Uri uri
    uri |> Download.asHtml >>= Manga.extractTitle |> Option.map (fun name -> Manga.create name uri)

let getChapters (manga:Manga) =
    Download.asHtml manga.Uri
    |>  Option.map Manga.extractChapters
    >>= Option.traverse manga.createChapterWithRelativeUri

let getPages (chapter:Chapter) =
    Download.asHtml chapter.Uri
    >>= Manga.extractPages
    >>= Option.traverse chapter.createPageWithRelativeUri

let getImage (page:Page) =
    page.Uri |> Download.asHtml >>= Manga.extractImage |> Option.map page.createImage


// Saving Images
let fileExtension fileName =
    Regex(".*\.(.*)$").Match(fileName).Groups.[1].Value

let getFileHandle manga chapter pageNo (image:Image) =
    let ext      = image.Uri.Segments |> Array.last |> fileExtension
    let fileName = sprintf "%d.%s" pageNo ext
    let path     = Path.Combine(manga, chapter, fileName)
    path |> Path.GetDirectoryName |> Dir.CreateDirectory |> ignore
    File.Open(path, System.IO.FileMode.Append, System.IO.FileAccess.Write)

let download (image:Image) (file:System.IO.FileStream) = maybe {
    let! uriSize = Download.size image.Uri
    if file.Position < uriSize then
        let req = Download.getRequest image.Uri
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
            printfn "%s" manga.Title
            match getChapters manga with
            | None          -> printfn "Error: Couldn't fetch Chapters"
            | Some chapters ->
                chapters |> Seq.iteri (fun i chapter ->
                    printfn "%d: %s" (i+1) chapter.Title
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
