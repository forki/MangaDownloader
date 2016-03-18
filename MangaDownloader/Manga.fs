namespace MangaDownloader
open FSharp.Data

module HTML =
    let extractTitle (html:HtmlDocument) =
        html.Descendants "div"
        |> Seq.filter  (HtmlNode.hasId "mangaproperties")
        |> Seq.collect (fun node -> node.Descendants "h2")
        |> Seq.map     (fun node -> node.InnerText())
        |> Seq.tryHead

    let extractChapters (html:HtmlDocument) =
        html.Descendants "div"
        |> Seq.filter  (HtmlNode.hasId "chapterlist")
        |> Seq.collect (fun node -> node.Descendants "a")
        |> Seq.map     (fun node ->
            let title = node.InnerText()
            let href  = node.AttributeValue("href")
            title, href
        )

    let extractPages (html:HtmlDocument) =
        html.Descendants "select"
        |> Seq.filter  (fun node -> node.HasId "pageMenu" )
        |> Seq.collect (fun node -> node.Descendants "option")
        |> Seq.map (fun node -> maybe {
            let! pageNumber = node.InnerText() |> System.Int32.tryParse
            let  href       = node.AttributeValue("value")
            return pageNumber, href
        })
        |> Option.sequence

    let extractImage (html:HtmlDocument) =
        html.Descendants "div"
        |> Seq.filter  (fun node -> node.HasId "imgholder" )
        |> Seq.collect (fun node -> node.Descendants "img")
        |> Seq.tryHead
        |> Option.map (HtmlNode.attributeValue "src" >> Uri)


[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Image =
    let (>>=) m f = Option.bind f m

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

    let getImage (page:Page) =
        page.Uri |> Download.asHtml >>= HTML.extractImage |> Option.map page.createImage

    let download (page:Page) = maybe {
        printfn "Downloading [%s] Page %d" page.Chapter.Title page.Number
        let! image = getImage page
        use file   = Image.fileHandle image
        do! Image.download image file
    }


[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Chapter =
    let (>>=) m f = Option.bind f m

    let getPages (chapter:Chapter) =
        Download.asHtml chapter.Uri
        >>= HTML.extractPages
        >>= Option.traverse chapter.createPageWithRelativeUri


[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Manga =
    let (>>=) m f = Option.bind f m

    let getManga uri =
        let uri = Uri uri
        uri |> Download.asHtml >>= HTML.extractTitle |> Option.map (fun name -> Manga.create name uri)

    let getChapters (manga:Manga) =
        Download.asHtml manga.Uri
        |>  Option.map HTML.extractChapters
        >>= Option.traverse manga.createChapterWithRelativeUri
