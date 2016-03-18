namespace MangaDownloader
open FSharp.Data

module HTML =
    let extractTitle (html:HtmlDocument) =
        html.Descendants "div"
        |> Seq.filter  (HtmlNode.hasId "mangaproperties")
        |> Seq.collect (fun node -> node.Descendants "h2")
        |> Seq.map     (fun node -> node.InnerText())
        |> Seq.tryHead

    let extractChapters (root:Uri) (html:HtmlDocument) =
        html.Descendants "div"
        |> Seq.filter  (HtmlNode.hasId "chapterlist")
        |> Seq.collect (fun node -> node.Descendants "a")
        |> Option.traverse (fun node -> maybe {
            let  title = node.InnerText()
            let! uri   = node.AttributeValue("href") |> Uri.tryCreate root
            return title, uri
        })

    let extractPages (root:Uri) (html:HtmlDocument) =
        html.Descendants "select"
        |> Seq.filter  (fun node -> node.HasId "pageMenu" )
        |> Seq.collect (fun node -> node.Descendants "option")
        |> Option.traverse (fun node -> maybe {
            let! pageNumber = node.InnerText() |> Int32.tryParse
            let! uri        = node.AttributeValue("value") |> Uri.tryCreate root
            return pageNumber, uri
        })

    let extractImage (html:HtmlDocument) =
        html.Descendants "div"
        |> Seq.filter  (fun node -> node.HasId "imgholder" )
        |> Seq.collect (fun node -> node.Descendants "img")
        |> Seq.tryHead
        |> Option.map (HtmlNode.attributeValue "src" >> Uri)


[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Image =
    let (>>=) m f = Option.bind f m

    let create page uri = {Page=page; Uri = uri}

    let fileExtension fileName =
        Regex(".*\.(.*)$").Match(fileName).Groups.[1].Value

    let fileHandle (image:Image) =
        let ext      = image.Uri.Segments |> Array.last |> fileExtension
        let fileName = sprintf "%03d.%s" image.Page.Number ext
        let path     = Path.Combine(image.Page.Chapter.Manga.Title, image.Page.Chapter.Title, fileName)
        path |> Path.GetDirectoryName |> Dir.CreateDirectory |> ignore
        File.Open(path, System.IO.FileMode.Append, System.IO.FileAccess.Write)

    let rawDownload (image:Image) (file:System.IO.FileStream) = maybe {
        let! uriSize = Download.size image.Uri
        if file.Position < uriSize then
            let req = Download.getRequest image.Uri
            req.AddRange(file.Position)
            let stream = req |> Download.asStream
            stream.CopyTo(file)
            return ()
    }

    let download image file = maybe {
        let! result = (rawDownload image |> Retry.fromException 5) file
        return! result
    }


[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Page =
    let (>>=) m f = Option.bind f m

    let create chapter no uri = {Chapter=chapter; Number = no; Uri = uri}

    let image (page:Page) =
        page.Uri |> Download.asHtml >>= HTML.extractImage |> Option.map (Image.create page)

    let download (page:Page) = maybe {
        printfn "Downloading [%s] Page %d" page.Chapter.Title page.Number
        let! image = image page
        use file   = Image.fileHandle image
        do! Image.download image file
    }


[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Chapter =
    let (>>=) m f = Option.bind f m

    let create manga title uri = {Manga=manga; Title = title; Uri = uri}

    let pages (chapter:Chapter) =
        Download.asHtml chapter.Uri
        >>= HTML.extractPages chapter.Uri
        |>  Option.map (Seq.map (fun (no,uri) -> Page.create chapter no uri))

    let download (chapter:Chapter) =
        chapter |> pages >>= Option.traverse Page.download |> Option.map ignore


[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Manga =
    let (>>=) m f = Option.bind f m

    let create title uri = {Manga.Title=title; Uri=uri}

    let fromUri uri =
        Download.asHtml uri >>= HTML.extractTitle |> Option.map (fun name -> create name uri)

    let chapters (manga:Manga) =
        Download.asHtml manga.Uri
        >>= HTML.extractChapters manga.Uri
        |>  Option.map (Seq.map (fun (title,uri) -> Chapter.create manga title uri))

    let private between x y i =
        i >= x && i <= y

    let downloadFromTo start stop (manga:Manga) =
        manga |> chapters |> Option.map (Seq.filterIndexed (fst >> between start stop)) 
        >>= Option.traverse Chapter.download |> Option.map ignore
