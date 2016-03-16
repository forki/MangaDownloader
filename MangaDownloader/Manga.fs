namespace MangaDownloader
open FSharp.Data

module Manga =
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


        
