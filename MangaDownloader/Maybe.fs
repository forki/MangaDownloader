namespace MangaDownloader

type IDisposable = System.IDisposable

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
        if not (guard()) then 
            Some () 
        else
            do f() |> ignore
            o.While(guard, f)
    member o.For(sequence:seq<_>, body) =
        o.Using(sequence.GetEnumerator(),
            fun enum -> o.While(enum.MoveNext, o.Delay(fun () -> body enum.Current)))

[<AutoOpen>]
module MaybeImport =
    let maybe = MaybeBuilder()
