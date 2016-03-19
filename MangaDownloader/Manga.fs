namespace MangaDownloader
open FSharp.Data

module HTML =
    let extractTitle (html:HtmlDocument) =
        html.Descendants "div"
        |> Seq.filter  (HtmlNode.hasId "mangaproperties")
        |> Seq.collect (fun node -> node.Descendants "h2")
        |> Seq.map     (fun node -> node.InnerText())
        |> Seq.tryHead 
        |> Result.fromOption (HTMLError ExtractTitle)

    let extractChapters (root:Uri) (html:HtmlDocument) =
        html.Descendants "div"
        |> Seq.filter  (HtmlNode.hasId "chapterlist")
        |> Seq.collect (fun node -> node.Descendants "a")
        |> Result.traverse (fun node -> result {
            let  title = node.InnerText()
            let! uri   = node.AttributeValue("href") |> Uri.tryCreate root |> Result.fromOption (HTMLError ChapterURL)
            return title, uri
        })

    let extractPages (root:Uri) (html:HtmlDocument) =
        html.Descendants "select"
        |> Seq.filter  (fun node -> node.HasId "pageMenu" )
        |> Seq.collect (fun node -> node.Descendants "option")
        |> Result.traverse (fun node -> result {
            let! pageNumber = node.InnerText() |> Int32.tryParse |> Result.fromOption (HTMLError ParseInt)
            let! uri        = node.AttributeValue("value") |> Uri.tryCreate root |> Result.fromOption (HTMLError PageURL)
            return pageNumber, uri
        })

    let extractImage (html:HtmlDocument) =
        html.Descendants "div"
        |> Seq.filter  (fun node -> node.HasId "imgholder" )
        |> Seq.collect (fun node -> node.Descendants "img")
        |> Seq.tryHead |> Result.fromOption (HTMLError ImageURL)
        |> Result.map (HtmlNode.attributeValue "src" >> Uri)


[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Image =
    let (>>=) m f = Result.bind f m

    let create page uri = {Page=page; Uri = uri}

    let fileExtension fileName =
        Regex(".*\.(.*)$").Match(fileName).Groups.[1].Value

    let fileHandle (image:Image) =
        let ext      = image.Uri.Segments |> Array.last |> fileExtension
        let fileName = sprintf "%03d.%s" image.Page.Number ext
        let path     = Path.Combine(image.Page.Chapter.Manga.Title, image.Page.Chapter.Title, fileName)
        path |> Path.GetDirectoryName |> Dir.CreateDirectory |> ignore
        File.Open(path, System.IO.FileMode.Append, System.IO.FileAccess.Write)

    let rawDownload (image:Image) (file:System.IO.FileStream) = result {
        let! uriSize = Download.size image.Uri
        if file.Position < uriSize then
            let req = Download.getRequest image.Uri
            req.AddRange(file.Position)
            let stream = req |> Download.asStream
            stream.CopyTo(file)
            return! Ok ()
        else
            return! Error (DownloadError (Fetch image.Uri))
    }

    let download image file =
        let rawDownload = rawDownload image |> Result.fromExn (DownloadError (Fetch image.Uri))
        Result.retry rawDownload 5 file |> Result.join
    


[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Page =
    let (>>=) m f = Result.bind f m

    let create chapter no uri = {Chapter=chapter; Number = no; Uri = uri}

    let image (page:Page) =
        page.Uri |> Download.asHtml >>= HTML.extractImage |> Result.map (Image.create page)

    let download (page:Page) = result {
        printfn "Downloading [%s] Page %d" page.Chapter.Title page.Number
        let! image = image page
        use file   = Image.fileHandle image
        return! Image.download image file
    }


[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Chapter =
    let (>>=) m f = Result.bind f m

    let create manga title uri = {Manga=manga; Title = title; Uri = uri}

    let pages (chapter:Chapter) =
        Download.asHtml chapter.Uri
        >>= HTML.extractPages chapter.Uri
        |>  Result.map (Seq.map (fun (no,uri) -> Page.create chapter no uri))

    let download (chapter:Chapter) =
        chapter |> pages >>= Result.traverse Page.download |> Result.map ignore


[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Manga =
    let (>>=) m f = Result.bind f m

    let create title uri = {Manga.Title=title; Uri=uri}

    let fromUri uri =
        Download.asHtml uri >>= HTML.extractTitle |> Result.map (fun name -> create name uri)

    let chapters (manga:Manga) =
        Download.asHtml manga.Uri
        >>= HTML.extractChapters manga.Uri
        |>  Result.map (Seq.map (fun (title,uri) -> Chapter.create manga title uri))

    let private between x y i =
        i >= x && i <= y

    let downloadFromTo start stop (manga:Manga) =
        manga |> chapters |> Result.map (Seq.filterIndexed (fst >> between start stop)) 
        >>= Result.traverse Chapter.download |> Result.map ignore
