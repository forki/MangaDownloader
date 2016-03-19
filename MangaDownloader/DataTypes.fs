namespace MangaDownloader

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
