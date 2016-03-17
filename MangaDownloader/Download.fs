﻿namespace MangaDownloader
open FSharp.Data

type WebRequest = System.Net.HttpWebRequest

module Download =
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

    let getRequest  uri = request uri |> setMethod "GET"
    let headRequest uri = request uri |> setMethod "HEAD"

    let asHtml uri =
        let load (uri:string) =
            HtmlDocument.Load(uri)
        (load |> Retry.fromException 5) <| uri.ToString()

    let rawSize (uri:Uri) =
        let req = headRequest uri
        use res = req.GetResponse()
        res.ContentLength

    let size (uri:Uri) = maybe {
        return! (rawSize |> Retry.fromException 5) uri
    }