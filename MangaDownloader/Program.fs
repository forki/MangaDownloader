open FSharp.Data

// Imported Types
type IDisposable = System.IDisposable
type Uri   = System.Uri
type File  = System.IO.File
type Dir   = System.IO.Directory
type Path  = System.IO.Path
type Regex = System.Text.RegularExpressions.Regex
type WebRequest = System.Net.HttpWebRequest


// Manga Types
type Image   = Image   of Uri
type Page    = Page    of int * Uri
type Chapter = Chapter of string * Uri
type Manga   = Manga   of string * Uri

let mangaTitle   (Manga   (title,_)) = title
let chapterTitle (Chapter (title,_)) = title
let pageNumber   (Page    (no,_))    = no


// Maybe Monad
let (>>=) m f = Option.bind f m
let (>>-) m f = Option.iter f m

type MaybeBuilder() =
    member o.Bind(m,f)     = Option.bind f m
    member o.Return(x)     = Some x
    member o.ReturnFrom(x) = x
    member o.Zero()        = None
    member o.Combine(m, f) = Option.bind f m
    member o.Delay(f: unit -> _) = f
    member o.Run(f) = f()
    member o.TryFinally(m, compensation) =
        try o.ReturnFrom(m)
        finally compensation()
    member o.Using(res:#IDisposable, body) =
        o.TryFinally(body res, fun () -> match res with null -> () | disp -> disp.Dispose())
    member o.While(guard, f) =
        if not (guard()) then Some () else
        do f() |> ignore
        o.While(guard, f)
    member o.For(sequence:seq<_>, body) =
        o.Using(sequence.GetEnumerator(),
            fun enum -> o.While(enum.MoveNext, o.Delay(fun () -> body enum.Current)))

let maybe = MaybeBuilder()


// Manuel HTTP functions
let request(uri:Uri) =
    let req = WebRequest.CreateHttp(uri)
    req.Timeout <- 5000
    req.ContinueTimeout <- 5000
    req.ReadWriteTimeout <- 5000
    req

let setMethod m (req:WebRequest) =
    req.Method <- m
    req

let asStream (req:WebRequest) = 
    req.GetResponse().GetResponseStream()

let requestGet  uri = request uri |> setMethod "GET"
let requestHead uri = request uri |> setMethod "HEAD"


// Retry function
let retry x f y =
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


// wrapper - needed because of object/type-inference
let loadHtml (uri:string) =
    HtmlDocument.Load(uri)


// Fetching/Parsing Manga
let getManga (uri:string) =
    let html = (loadHtml |> retry 5) uri
    html |> Option.map (fun html ->
        let name =
            html.Descendants "div"
            |> Seq.filter  (HtmlNode.hasId "mangaproperties")
            |> Seq.collect (fun node -> node.Descendants "h2")
            |> Seq.map     (fun node -> node.InnerText())
            |> Seq.head
        Manga (name, Uri uri)
    )

let getChapters (Manga (name,uri)) =
    let html = (loadHtml |> retry 5) <| uri.ToString()
    html |> Option.map (fun html ->
        html.Descendants "div"
        |> Seq.filter  (HtmlNode.hasId "chapterlist")
        |> Seq.collect (fun node -> node.Descendants "a")
        |> Seq.map     (fun node ->
            let title  = node.InnerText()
            let _,href = Uri.TryCreate(uri, node.AttributeValue("href"))
            Chapter (title, href)
        )
    )

let getPages (Chapter (chapter,uri)) =
    let html = (loadHtml |> retry 5) <| uri.ToString()
    html |> Option.map (fun html ->
        html.Descendants "select"
        |> Seq.filter  (fun node -> node.HasId "pageMenu" )
        |> Seq.collect (fun node -> node.Descendants "option")
        |> Seq.map (fun node ->
            let pageNumber = node.InnerText() |> System.Int32.Parse
            let _,href     = Uri.TryCreate(uri, node.AttributeValue("value"))
            Page (pageNumber, href)
        )
    )

let getImage (Page (no,uri)) =
    let html = (loadHtml |> retry 5) <| uri.ToString()
    html |> Option.bind (fun html ->
        html.Descendants "div"
        |> Seq.filter  (fun node -> node.HasId "imgholder" )
        |> Seq.collect (fun node -> node.Descendants "img")
        |> Seq.tryHead
        |> Option.map (fun node -> node.AttributeValue "src")
        |> Option.map (Uri >> Image)
    )

let fileExtension fileName =
    Regex(".*\.(.*)$").Match(fileName).Groups.[1].Value

let getFileHandle manga chapter pageNo (Image uri) =
    let ext      = uri.Segments |> Array.last |> fileExtension
    let fileName = sprintf "%d.%s" pageNo ext
    let path     = Path.Combine(manga, chapter, fileName)
    path |> Path.GetDirectoryName |> Dir.CreateDirectory |> ignore
    File.Open(path, System.IO.FileMode.Append, System.IO.FileAccess.Write)

let getUriSize (uri:Uri) =
    let req = requestHead uri
    use res = req.GetResponse()
    res.ContentLength

let download (Image uri) (file:System.IO.FileStream) =
    let uriSize = (getUriSize |> retry 5) uri
    uriSize |> Option.iter (fun uriSize ->
        if file.Position < uriSize then
            use stream = requestGet uri |> asStream
            stream.CopyTo(file)
    )

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
        let  no           = System.Int32.Parse no
        let! manga        = getManga manga
        let  mangaTitle   = mangaTitle manga
        let! chapters     = getChapters manga
        let  chapters     = Array.ofSeq chapters
        let  chapter      = chapters.[no-1]
        let  chapterTitle = chapterTitle chapter
        let! pages        = getPages chapter
        for page in pages do
            let pageNumber = pageNumber page
            printfn "Downloading [%s] Page %d" chapterTitle pageNumber
            let! image = getImage page
            let file = getFileHandle mangaTitle chapterTitle pageNumber image
            (download image |> retry 5) file |> ignore
            file.Dispose()
    }

    let downloadChapters manga start ``end`` = maybe {
        let  start      = (System.Int32.Parse start)   - 1
        let  ``end``    = (System.Int32.Parse ``end``) - 1
        let! manga      = getManga manga
        let  mangaTitle = mangaTitle manga
        let! chapters   = getChapters manga
        let  chapters   = Array.ofSeq chapters
        for i in start .. ``end`` do
            let chapter      = chapters.[i]
            let chapterTitle = chapterTitle chapter
            let! pages = getPages chapter
            for page in pages do
                let pageNumber = pageNumber page
                printfn "Downloading [%s] Page %d" chapterTitle pageNumber
                let! image = getImage page
                let  file  = getFileHandle mangaTitle chapterTitle pageNumber image
                (download image |> retry 5) file |> ignore
                file.Dispose()
    }

[<EntryPoint>]
let main argv =
    match argv with
    | [| manga |]       -> Console.showChapters manga
    | [| manga; no |]   -> Console.downloadChapter manga no |> ignore
    | [| manga; s; e |] -> Console.downloadChapters manga s e |> ignore
    | _                 -> Console.showUsage ()
    0 // return an integer exit code
