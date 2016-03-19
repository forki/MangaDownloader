namespace MangaDownloader

type Result<'a,'b> =
    | Ok    of 'a
    | Error of 'b

module Result =
    let map f x =
        match x with
        | Error x -> Error x
        | Ok x    -> Ok (f x)

    let bind f x =
        match x with
        | Error x -> Error x
        | Ok x    -> f x

    let onError f x =
        match x with
        | Error x -> f x
        | Ok x    -> Ok x

    let fromOption msg x =
        match x with
        | None   -> Error msg
        | Some x -> Ok x

    let apply fx xx =
        match fx,xx with
        | Ok f, Ok    x    -> Ok (f x)
        | Ok _, Error x    -> Error x
        | Error x, Ok _    -> Error x
        | Error x, Error _ -> Error x

    let (<*>) = apply

    let traverse f sequence =
        let cons x s    = seq { yield! s; yield x }
        let cons x s    = Ok cons <*> x <*> s
        let folder xs x = cons (f x) xs
        Seq.fold folder (Ok Seq.empty) sequence

    let sequence s = traverse id s

    let fromExn msg f x =
        try  Ok <| f x
        with exn -> Error msg

    let fromExns handler f x =
        try  Ok <| f x
        with exn -> handler exn

    let join x =
        match x with
        | Error x -> Error x
        | Ok x ->
            match x with
            | Error x -> Error x
            | Ok x    -> Ok x

    let retry f amount x =
        let rec loop count =
            match f x with
            | Ok x    -> Ok x
            | Error x ->
                match count < amount with
                | true  -> loop (count+1)
                | false -> Error x
        loop 0

type ResultBuilder() =
    member o.Bind(m,f)     = Result.bind f m
    member o.Return(x)     = Ok x
    member o.ReturnFrom(x) = x
    member o.Zero()        = Error ()
    member o.Combine(m, f) = Result.bind f m
    member o.Delay(f: unit -> _) = f
    member o.Run(f) = f()
    member o.TryFinally(m, compensation) =
        try o.ReturnFrom(m)
        finally compensation()
    member o.Using(res:#IDisposable, body) =
        o.TryFinally(body res, fun () -> match res with null -> () | disp -> disp.Dispose())
    member o.While(guard, f) =
        if not (guard()) then 
            Ok () 
        else
            do f() |> ignore
            o.While(guard, f)
    member o.For(sequence:seq<_>, body) =
        o.Using(sequence.GetEnumerator(),
            fun enum -> o.While(enum.MoveNext, o.Delay(fun () -> body enum.Current)))

[<AutoOpen>]
module ResultImport =
    let result = ResultBuilder()
