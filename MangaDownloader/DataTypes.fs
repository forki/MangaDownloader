namespace MangaDownloader

// All kind of application errors that could happen
type HTMLErrors =
    | ParseInt
    | ImageURL
    | PageURL
    | ChapterURL
    | ExtractTitle

type DownloadErrors =
    | Size  of Uri
    | Fetch of Uri

type MangaError =
    | HTMLError     of HTMLErrors
    | DownloadError of DownloadErrors

// Easier way to create error types
module Error =
    module HTML =
        let parseInt     = HTMLError ParseInt
        let imageURL     = HTMLError ImageURL
        let pageURL      = HTMLError PageURL
        let chapterURL   = HTMLError ChapterURL
        let extractTitle = HTMLError ExtractTitle
    module Download =
        let size  = Size  >> DownloadError
        let fetch = Fetch >> DownloadError
    
    // Converts error types to strings
    let toString = function
        | HTMLError ParseInt     -> "Couldn't parse a string to an int"
        | HTMLError ImageURL     -> "Parsing Image URL failed"
        | HTMLError PageURL      -> "Parsing Page URL failed"
        | HTMLError ChapterURL   -> "Parsing Chapter URL failed"
        | HTMLError ExtractTitle -> "Parsing Manga Title failed"
        | DownloadError (Size uri)  -> sprintf "Fetching Size of %O failed" uri
        | DownloadError (Fetch uri) -> sprintf "Fetching %O failed" uri

// The types representing the Mangas
type Manga = {
    Title: string
    Uri:   Uri
}

type Chapter = {
    Manga: Manga
    Title: string
    Uri:   Uri
}

type Page = {
    Chapter: Chapter
    Number:  int
    Uri:     Uri
}

type Image = {
    Page: Page
    Uri:  Uri
}
